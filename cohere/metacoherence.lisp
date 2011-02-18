   ; File: metacoherence.lisp
   ; Purpose: metacoherence algorithms
   ; Programmer: Paul Thagard, 10-97
   ; See also measure.lisp and cohere.lisp

   (defvar *emotion-nodes* '(coherence surprise incoherence happiness anxiety
   sadness)) ; + fear disgust anger

   ; METACO takes a settled HOTCO network and applies various metacoherence
   and emotional functions

   (defun metaco ()
   (coherence-nodes) ; activate nodes for overall coherence
   (surprise-node) ; activate node for overall surpise
   (valence-nodes) ; activated nodes for overall valence - happiness, sadness
   ;(fear-node) ; activate node for fear
   ;(disgust-node) ; activate node for disgust
   ;(anger-node) ; activate node for anger
   (update-emotions) ; adjust emotions in relation to each other and bodily
   input
   )

   ; UPDATE-EMOTIONS
   (defun update-emotions ()
   (setf *all-units* *emotion-nodes*)
   (setf *settled?* nil)
   (initialize-emotions)
   (run-hyp-net)
   (mapcar #'pl *emotion-nodes*)
   )

   ; INITIALIZE-EMOTIONS GIVES A DEFAULT ACTIVATION TO A UNIT IF IT DOESN'T
   HAVE ONE

   (defun initialize-emotions ()
   (dolist (node *emotion-nodes*)
   (unless (get node 'activation)
   (setf (get node 'activation) *default-activ*) .01
   )
   )
   )

    

   ; EMOTION-NETWORK creates a network of inhibitory and excitatory units
   representing
   ; emotion nodes.
   ; This could be expanded to include variables for bodily states like heart
   rate.

   (defun emotion-network ()
   (associate 'coherence 'incoherence -1)
   (associate 'coherence 'happiness 1)
   (associate 'incoherence 'anxiety 1)
   (associate 'happiness 'sadness -1)
   (associate 'happiness 'anxiety -1)
   )

    

   ; COHERENCE-NODES measures coherence of a solution in a connectionist
   fashion.
   ; Each node reports to nodes COHERENCE and INCOHERENCE. If a unit is
   active,
   ; then the number of constraints satisfied activate the COHERENCE node,
   ; and the number of constraints unsatisfied activated the INCOHERENCE
   node.
   ; (A more complicated way of doing this might use goodness.)
   ; Perhaps the contribution of a unit to coherence should be a factor of
   both its activation
   ; and its degree of constraint satisfaction.
   ; Perhaps this could use *all-units* instead of just active units.
   ; No - want to measure the coherence of the accepted part of the solution.

   (defun coherence-nodes ()
   (let ((active-units (active-units)))
   (do ((units active-units (cdr units))
   (net-size (length active-units)) ; normalize
   (coh-value 0) ; contribution to coherence: measure of constraint
   satisfaction
   (incoh-value 0) ; contribution to incoherence: measure of constraints not
   satisfied
   (constraint-satisfaction nil) ; list of two numbers representing weights
   of constraints satisfied and not satisfied
   )
   ;exit: activate coherence nodes
   ((null units)
   (setf (get 'coherence 'activation) (/ coh-value net-size))
   (setf (get 'incoherence 'activation) (/ incoh-value net-size))
   )
   ;repeat:
   (setf constraint-satisfaction (evaluate-constraints (car units))) ; look
   at constraints satisfied and not satisfied
   ; (my-print (car units) constraint-satisfaction)
   (setf coh-value
   (+ coh-value
   (first constraint-satisfaction)
   )
   )
   (setf incoh-value
   (+ incoh-value
   (second constraint-satisfaction)
   )
   )
   )
   )
   )

   ; ACTIVE-UNITS lists units with positive activation.
   (defun active-units ()
   (let ((result nil))
   (dolist (unit *all-units* result)
   (if (plusp (get unit 'activation))
   (push unit result)
   )
   )
   )
   )

   ; EVALUATE-CONSTRAINTS reports for a unit how well it's contraints are
   satisfied.
   ; Unlike coh-score from cohere.lisp, it is unit-oriented instead of
   constraint oriented.
   ; The solution used here is by default the connectionist one, but other
   solutions could also be
   ; used. Returns a list: (coherence-value incoherence-value).
   ; Both values reflect activation as well as constraints satisfied.

   (defun evaluate-constraints (unit)
   (do ((links (links-from unit) (cdr links))
   (weight-sat 0) ; weight of satisfied constraints
   (weight-not-sat 0) ; weight of unsatisfied constraints
   (accepted (fourth *connect-solution*)) ; default
   (rejected (fifth *connect-solution*))
   (link nil)
   )
   ; exit
   ((null links) (list weight-sat weight-not-sat)) ; return total weights
   ; repeat
   (setf link (car links))
   (if (plusp (cons-satisfied (cons unit link) accepted rejected))
   ; constraint is satisfied - function is from cohere.lisp
   (setf weight-sat
   (+ weight-sat (abs (cdr link)))
   )
   ; else constraint not satisfied
   (setf weight-not-sat
   (+ weight-not-sat (abs (cdr link)))
   )
   )
   )
   )

   ; VALENCE-NODES uses two nodes to sum up the overall valence of accepted
   ; units. Each active unit with postive valence passes it to the happiness
   node. ;
   ; Each active unit with negative valence passes it to the sadness node.
   ; Activation passed is proportional to the activation of the node. Cf.
   expected utility.

   (defun valence-nodes ()
   (let ((active-units (active-units)))
   (do ((units active-units (cdr units))
   (net-size (length active-units)) ; normalize
   (pos-valence 0)
   (neg-valence 0)
   (unit-valence 0)
   )
   ;exit: activate valence nodes
   ((null units)
   (setf (get 'happiness 'activation) (/ pos-valence net-size))
   (setf (get 'sadness 'activation) (/ neg-valence net-size))
   )
   ;repeat:
   (setf unit-valence (or (get (car units) 'valence) 0)) ; check that unit
   has valence
   (if (plusp (get (car units) 'valence)) ; positive valence
   (setf pos-valence
   (* (+ pos-valence unit-valence)
   (get (car units) 'activation)
   )
   )
   ; else negative valence
   (setf neg-valence
   (* (+ neg-valence unit-valence)
   (get (car units) 'activation)
   )
   )
   )
   )
   )
   )

    

    

   ; STORE-ACTIVATIONS stores the activations from a connectionist run in
   order to
   ; notice surprise when successive runs produce different answers.

   (defvar *stored-activations* nil "Stores activations of a run for
   comparison.")

   (defun store-activations ()
   (do ((units *all-units* (cdr units)))
   ;exit:
   ((null units) (my-print "Activations stored."))
   ;repeat:
   (setq *stored-activations*
   (acons (car units)
   (get (car units) 'new-activation)
   *stored-activations*
   )
   )
   )
   )

   ; SURPRISE-NODE looks for changes in activation from one run to the next,
   with the
   ; total sum of surprise being used to activate the surprise node.

   (defun surprise-node ()
   (do ((units *all-units* (cdr units))
   (sum-activation-change 0) ; total amount of activation difference
   )
   ;exit:
   ((null units)
   (setf (get 'surprise 'activation) sum-activation-change)
   )
   ;repeat:
   (setf sum-activation-change
   (+ sum-activation-change
   (actdiff (get (car units) 'new-activation)
   (get-stored-activation (car units))
   )
   )
   )
   )
   )

   ; SIGN-DIFFERENCE returns t if one number is greater than 0 and the other
   is less.
   (defun sign-difference (num1 num2)
   (or (and (> num1 0) (< num2 0))
   (and (< num1 0) (> num2 0))
   )
   )

   ; GET-STORED-ACTIVATION gets an activation from the association list
   *stored-activations*.

   (defun get-stored-activation (unit)
   (or (cdr (assoc unit *stored-activations*))
   (get unit 'activation) ; no previous run
   )
   )

   ; ACTDIFF gives the absolute difference between two activation values,
   which gets
   ; tricky if one is positive and the other negative.

   (defun actdiff (n1 n2)
   (if (sign-difference n1 n2) ; + and -
   (+ (abs n1) (abs n2))
   ; else same signs
   (abs (- n1 n2))
   )
   )

    

   ; HOW-COHERENT looks at the global coherence of a solution.
   ; It considers both the non-connectionist measure based on coh-score
   ; and the connectionist measure based on mean goodness (harmony).

    

   (defun how-coherent ()
   (my-print "Connectionist solution is " *connect-solution*)
   (my-print "This solution satisfies constraints:")
   (my-print " Weight: " (second *connect-solution*))
   (my-print " Constraints: " (third *connect-solution*))
   (my-print "The mean goodness of units is: " (mean-goodness *all-units*))
   (my-print "The mean goodness of active units is: " (mean-goodness
   (active-units)))
   (my-print "The overall suprise was " (surprise))

   )
   (defun hc () (how-coherent))

   ; SURPRISE looks for changes in activation from one run to the next.
   ; It's more global than surprise-node, and it only counts units whose
   activation has changed sign.

   (defun surprise ()
   (do ((units *all-units* (cdr units))
   (changed-units nil) ; units changing acceptance status
   (sum-activation-change 0) ; total amount of activation difference
   )
   ;exit:
   ((null units)
   (my-print (length changed-units) " changed: " changed-units)
   (my-print "Total activation change: " sum-activation-change)
   )
   ;repeat:
   (if (sign-difference (get (car units) 'new-activation)
   (get-stored-activation (car units))
   )
   (push (car units) changed-units)
   )
   (setf sum-activation-change
   (+ sum-activation-change
   (actdiff (get (car units) 'new-activation)
   (get-stored-activation (car units))
   )
   )
   )
   )
   )
