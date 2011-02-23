(define maximum-iterations (make-parameter 5000))

(define decay (make-parameter 0.05))

(define epsilon (make-parameter 0.001))

(define initial-activation (make-parameter (default-activation)))

(define (display-problem/dot problem)
  (define (activation->saturation activation)
    (- 255 (inexact->exact (floor (* (/ activation (if (positive? activation)
                                                       (maximum-activation)
                                                       (minimum-activation)))
                                     255)))))
  
  (let ((document "digraph G {
                     graph [size=\"12!\", ratio=0.618033989];
                     node [style=filled]; edge [dir=none];
                     ~a
                     ~a
                   }"))
    ;; what about writing right away, and not gathering?
    (let ((propositions
           (hash-table-fold
            (problem-propositions problem)
            (lambda (name proposition nodes)
              (cons
               (let* ((activation (proposition-activation proposition))
                      (saturation (activation->saturation activation)))
                 (format "\"~a\" [label=\"~a\", fillcolor=\"#~a~a~a\"];"
                         name
                         (let ((description
                                (proposition-description proposition)))
                           (if (default-description? description)
                               name
                               #;description
                               name
                               ))
                         (format "~2,48X" (if (negative? activation)
                                              255
                                              saturation))
                         (format "~2,48X" (if (positive? activation)
                                              255
                                              saturation))
                         (format "~2,48X" saturation)))
               nodes))
            '()))
          ;; respect sign and scale with dashed/solid and thickness?
          (constraints
           (map
            (match-lambda
                ((whence whither . weight)
                 (format "\"~a\" -> \"~a\" [style=~a];"
                         whence
                         whither
                         (if (positive? weight)
                             "solid"
                             "dashed"))))
            ;; it's damn-expensive to delete one edge in a
            ;; bidirectional graph; we might want to consider
            ;; switching to a unidirectional graph, simulating
            ;; bidirectionality.
            (delete-duplicates
             (hash-table-fold
              (problem-constraints problem)
              (lambda (name constraints edges)
                (append
                 (map (lambda (constraint)
                        (cons* name
                               (proposition-name
                                (constraint-whither constraint))
                               (constraint-weight constraint)))
                      constraints)
                 edges))
              '())
             (lambda (edge-a edge-b)
               (match-let (((a-whence a-whither . a-weight) edge-a)
                           ((b-whence b-whither . b-weight) edge-b))
                 (and (eq? a-whence b-whither)
                      (eq? b-whence a-whither))))))))
      (format #t
              document
              (string-join propositions)
              (string-join constraints)))))

(define (clamp low high value)
  (max low (min high value)))

(define cardinality
  (case-lambda
   ((integer) (cardinality integer 10))
   ((integer base)
    (loop ((for power (up-from 0))
           (until (> (expt base power) integer)))
          => power))))

(defstruct processor pre-process process post-process)

(define (make-animation-processor animation)
  (let ((temp-dir (create-temporary-directory))
        (temp-digits (cardinality (maximum-iterations))))
    (let ((output-template
           (format "~~~a,48d.gif" temp-digits)))
      (make-processor
       pre-process: noop
       process:
       (lambda (problem iteration)
         (let-values
             (((in out id)
               (process
                "dot" `("-Tgif"
                        "-o" ,(make-pathname temp-dir
                                             (format output-template
                                                     iteration))))))
           (close-input-port in)
           (with-output-to-port
               out
             (lambda () (display-problem/dot problem)))
           (close-output-port out)))
       post-process:
       (lambda (problem)
         (run (convert "$(" find ,temp-dir -name \'*.gif\' \| sort &&
                       find ,temp-dir -name \'*.gif\' \| sort -r ")"
                       -loop 0 ,animation)))))))

(define (make-time-series-processor time-series)
  (let ((time-series-data (create-temporary-file)))
    (make-processor
     pre-process: noop
     process:
     (let ((time-series-data (open-output-file time-series-data)))
       (lambda (problem iteration)
         (display (string-join
                   (map
                    (compose number->string proposition-activation)
                    (hash-table-values (problem-propositions problem))))
                  time-series-data)
         (newline time-series-data)))
     post-process:
     (lambda (problem)
       (let ((propositions (problem-propositions problem))
             (document "set term pngcairo size 1024,768 font \",8\" enhanced crop;
                         set output \"~a\";
                         plot ~a~%"))
         (let ((elements
                (loop ((for name (in-list (hash-table-keys propositions)))
                       (for proposition
                            (in-list (hash-table-values propositions)))
                       (for column (up-from 1))
                       (for elements
                            (listing
                             (format "\"~a\" using ~a with lines title \"~a\""
                                     time-series-data
                                     column
                                     name))))
                      => elements)))
           (let-values (((in out id) (process "gnuplot")))
             (close-input-port in)
             (format
              out
              document
              time-series
              (string-join elements ", "))
             (close-output-port out))))))))

(define solve!
  (case-lambda
   ((algorithm) (solve! algorithm (current-problem)))
   ((algorithm problem) (algorithm problem))))

(define (connectionist . processors)
  (lambda (problem)
    (let ((propositions (problem-propositions problem))
          (constraints (problem-constraints problem)))
      (for-each (lambda (processor)
                  ((processor-pre-process processor) problem))
                processors)
      (hash-table-walk
       propositions
       (lambda (name proposition)
         (if (not (proposition-evidence? proposition))
             (proposition-activation-set! proposition (initial-activation)))))
      (let iterate ((iteration 0)
                    (delta +Inf))
        (if (or (< (abs delta) (epsilon))
                (> iteration (maximum-iterations)))
            (begin
              ;; one last normal processing at t_n
              (for-each (lambda (processor)
                          ((processor-process processor) problem iteration))
                        processors)
              (for-each (lambda (processor)
                          ((processor-post-process processor) problem))
                        processors)
              problem)
            (let ((activations
                   (hash-table-fold
                    propositions
                    (lambda (name proposition activations)
                      (let ((activation
                             (if (proposition-evidence? proposition)
                                 (proposition-activation proposition)
                                 (let ((incoming-activation
                                        (let ((constraints (hash-table-ref constraints name)))
                                          (apply +
                                                 (map *
                                                      (map constraint-weight constraints)
                                                      (map (compose proposition-activation
                                                                    constraint-whither)
                                                           constraints)))))
                                       (activation (proposition-activation proposition)))
                                   (+ (* activation (- 1 (decay)))
                                      (* incoming-activation
                                         (if (positive? incoming-activation)
                                             (- (maximum-activation) activation)
                                             (- activation (minimum-activation)))))))))
                        (alist-cons name
                                    (clamp (minimum-activation)
                                           (maximum-activation)
                                           activation)
                                    activations)))
                    '()))) 
              (for-each (lambda (processor)
                          ((processor-process processor) problem iteration))
                        processors)
              ;; do we need this read step, or can we simply update and take
              ;; the delta? let's update and take the delta. no; unless we
              ;; mutate, we need to read the delta.
              (let ((delta
                     (apply +
                            (map (match-lambda
                                     ((name . activation)
                                      (- activation
                                         (proposition-activation
                                          (hash-table-ref propositions name)))))
                                 activations)))) 
                (for-each (match-lambda
                              ((name . activation)
                               (hash-table-update! propositions
                                                   name
                                                   (lambda (proposition)
                                                     (proposition-activation-set!
                                                      proposition
                                                      activation)
                                                     proposition))))
                          activations)
                (iterate (add1 iteration)
                         delta))))))))
