(module
 influence
 *
 (import scheme
         chicken
         data-structures
         files
         ports)

 (use srfi-1
      srfi-13
      srfi-69
      posix
      defstruct
      matchable
      format
      foof-loop
      shell)

 (define default-activation (make-parameter 0.01))
 (define maximum-activation (make-parameter 0.99))
 (define minimum-activation (make-parameter -0.99))

 (define excitatory (make-parameter 0.04))
 (define inhibitory (make-parameter -0.06))
 (define default-weight (make-parameter (excitatory)))

 (define default-description (make-parameter ""))

 (define (default-description? description)
   (string=? description (default-description)))

 (defstruct proposition
   name
   (activation (default-activation))
   (description (default-description))
   ;; propositions are trivially accepted, rejected or something
   ;; orthogonal? a default of #t is trivially optimistic.
   (accepted? #t)
   (evidence? #f))

 (define evidence
   (make-proposition
    name: 'evidence
    activation: (maximum-activation)
    description: "Evidence"
    evidence?: #t))

 (define (evidence? proposition)
   (eq? evidence proposition))

 (defstruct constraint
   (weight (default-weight))
   whither)

 (defstruct problem
   (propositions
    ;; should we rethink this at some point?
    (let ((propositions (make-hash-table eq? hash-by-identity))) 
      (hash-table-set! propositions 'evidence evidence)
      propositions))
   (constraints (make-hash-table eq? hash-by-identity)))

 (define current-problem (make-parameter (make-problem)))

 (define link!
   (case-lambda
    ((problem whence whither)
     (link! problem whence whither (default-weight)))
    ((problem whence whither weight)
     (let* ((whither (hash-table-ref (problem-propositions problem)
                                     whither))
            (constraint (make-constraint weight: weight
                                         whither: whither)))
       (hash-table-update!/default
        (problem-constraints problem)
        whence
        (cut cons constraint <>)
        '())))))

 ;; explain, contradict should call link-symmetrically! with
 ;; e.g. certain default weights.
 (define link-symmetrically!
   (case-lambda
    ((problem whence whither)
     (link-symmetrically! problem whence whither (default-weight)))
    ((problem whence whither weight)
     (link! problem whence whither weight)
     (link! problem whither whence weight)))) 

 (define cohere!
   (case-lambda
    ((cohaeretors cohaerendum)
     (cohere! cohaeretors cohaerendum (default-weight)))
    ((cohaeretors cohaerendum weight)
     (cohere! (current-problem) cohaeretors cohaerendum weight))
    ((problem cohaeretors cohaerendum weight)
     (for-each
      (cut link-symmetrically! problem cohaerendum <> weight)
      cohaeretors))))

 ;; or maybe explain should do the symbol lookup, etc.
 (define explain!
   (case-lambda
    ((explanators explanandum)
     (explain! (current-problem) explanators explanandum))
    ((problem explanators explanandum)
     (cohere! problem explanators explanandum (excitatory)))))

 (define contradict!
   (case-lambda
    ((contradictors contradictum)
     (contradict! (current-problem) contradictors contradictum))
    ((problem contradictors contradictum)
     (cohere! problem contradictors contradictum (inhibitory)))))

 (define evidence!
   (case-lambda
    ((evidenced)
     (evidence! (current-problem) evidenced))
    ((problem evidenced)
     (explain! evidenced 'evidence))))

 (define propose!
   (case-lambda
    ((name)
     (propose! name (default-description)))
    ((name description)
     (propose! name description (default-activation)))
    ((name description activation)
     (propose! (current-problem) name description activation))
    ((problem name description activation)
     (hash-table-set!
      (problem-propositions problem)
      name
      (make-proposition name: name
                        activation: activation
                        description: description)))))

 (define (with-problem problem thunk)
   (parameterize ((current-problem problem)) (thunk)))

 (include "connectionist.scm"))
