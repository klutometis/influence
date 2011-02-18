   ; File: measure.lisp
   ; Purpose: algorithms for measuring coherence
   ; Programmer: Paul Thagard, 2-97
   ; updated 11-97 to included goodness (harmony)

   ; COH-SCORE-DUP calculates the coherence scores
   ; of a set of units partitioned into accepted and
   ; rejected. It returns a list of two scores
   ; %weight - the fraction of the total weight satisfied
   ; %constraints - the fraction of total constraints satisfied
   ; Input is a triple (number accepted rejected)
   ; This is just like coh-score except that it does not try to avoid
   ; counting constraints twice, and it produces a raw sum of constraints
   ; satified rather than a ratio.

   (defun coh-score-dup (solution)
   (setq *all-constraints* (list-constraints-dup *all-units*)) ; note dup
   (let ((num-satisfied 0)
   (weights-satisfied 0)
   (num-constraints (length *all-constraints*))
   (constraint-weight 0)
   )
   (dolist (constraint *all-constraints*)
   (setq constraint-weight
   (cons-satisfied constraint (second solution) (third solution))
   )
   (if (> constraint-weight 0)
   ; if constraint is satisfied by the accepted/rejected partition, then
   (and (setq num-satisfied (1+ num-satisfied))
   (setq weights-satisfied (+ constraint-weight weights-satisfied))
   )
   )
   )
   weights-satisfied
   )
   )

   ; LIST-CONSTRAINTS-DUP generates a list of all constraints using the links
   associated
   ; with each unit (element) in a list. Each constraint has the structure
   ; (unit1 unit2 weight). It is just like list-constraints in cohere.lisp,
   ; except that it does [not?] avoid counting constraint twice.

   (defun list-constraints-dup (list-of-units)
   (do ((units list-of-units (cdr units))
   (constraints nil)
   (unit nil)
   (units-done nil)
   ) ; variables
   ((null units) constraints) ; result
   ; repeat:
   (dolist (link (links-from (car units)))
   (setq unit (car link))
   ;(unless (member unit units-done) ; unless clause deleted
   (push (cons (car units) link) constraints)
   )
   (push (car units) units-done)
   )
   )

   ; COH-SCORE-SUBSET calculates the coherence of a subset of elements,
   relative
   ; to a partition into accepted and rejected. Like coh-score-dup, it
   ; can count constraints twice, e.g. when both e1 and e2 are in the subset
   ; and there is an positive constraint between them and both are accepted.
   ; Unlike coh-score which returns ratios, it returns a raw score of
   constraints
   ; satisfied.

   (defun coh-score-subset (elements solution)
   (let ((num-satisfied 0)
   (weights-satisfied 0)
   (num-constraints (length *all-constraints*))
   (constraint-weight 0)
   )
   (dolist (constraint (list-constraints-dup elements))
   (setq constraint-weight
   (cons-satisfied constraint (second solution) (third solution))
   )
   (if (> constraint-weight 0)
   ; if constraint is satisfied by the accepted/rejected partition, then
   (and (setq num-satisfied (1+ num-satisfied))
   (setq weights-satisfied (+ constraint-weight weights-satisfied))
   )
   )
   )
   weights-satisfied
   )
   )

   ;================GOODNESS===================
   ; This is originally from run.l in old ACME.

   ; GOODNESS is a measure of how well constraints are satisfied..
   ; Returns 1 if no units. Need to divide by 2 since all
   ; links get counted twice.

   (defun goodness (list-of-units)
   (do ((units list-of-units (cdr units))
   (value 0)
   )
   ;exit:
   ((null units) (if (null list-of-units) 1 (/ value 2)))
   ;repeat:
   (setq value
   (+ value
   (how-good-unit (car units) (get (car units) 'links-from))
   )
   )
   )
   )

   ; MEAN-GOODNESS cancels the effect of large networks

   (defun mean-goodness (units) (/ (goodness units) (length units)))
   ; ****************************************************
   ; HOW-GOOD-UNIT calculates the goodness of a particular unit
   ; with respect to its associates. List-pairs is now a
   ; list of dotted pairs (unit . weight).

   (defun how-good-unit (unit list-pairs)
   (do ((pairs list-pairs (cdr pairs))
   (value 0)
   )
   ((null pairs) value)
   (setq value (+ value
   (* (cdar pairs) ; weight
   (get unit 'activation)
   (get (caar pairs) 'activation)
   )
   )
   )
   )
   )
   ; Note: what this does is up goodness if the activation of units
   ; with high weights between them is high.

   (defun gu (unit)
   (my-print '"Goodness of " unit '" is "
   (how-good-unit unit (get unit 'links-from))
   )
   )

   (defun gum () (mapcar #'gu *all-units*))

    
