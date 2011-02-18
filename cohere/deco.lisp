   ; FILE: deco.lisp
   ; PURPOSE: basic functions for DECO: deliberative coherence
   ; Modified by PT, 5-20-93

    

    

   ; *********************************
   ; * FUNCTIONS FOR PARSING DECO INPUT
   ; *********************************

   ;GOAL Calls "proposition" to set up a node and puts a property
   ; to that atom that it is a goal.
   ; This will cause the running functions to not allow the value to go below
   0.

   (defun goal(name sentence &optional priority)
   (proposition name sentence)
   (put-prop 'name 'goal 'type)
   (if priority (make-sym-link 'special name (* priority *goal-excit*)))
   (print-si name " represents goal: " sentence " with priority " priority)
   )

   ;ACTION
   (defun action (name sentence)
   (proposition name sentence)
   (put-prop 'name 'action 'type)
   (print-si name " represents action: " sentence)
   )

   ; PROPOSITION sets up the units representing the various goals. It creates
   ; a unit for each proposition with *init-activ* as its initial activation,
   ; and stores the string defining the unit as a property of the unit.
   (defun proposition (name sentence)
   (setf (prop name) sentence)
   (pushnew name *all-propositions*)
   (note-unit name)
   (setf (activation name) *init-activ*)
   (setf (original-activation name) *init-activ*)
   )

   ; FACILITATE establishes the coherence links between units that facilitate
   ; other units. The optional argument VALUE allows a goal or set of goals
   ; to facilitate another goal with varied strength. SUBGOALS may be either
   ; an atom or a list, but SUPERGOAL must be an atom. If multiple SUBGOALS
   ; are needed to facilitate a goal, the strength of their links are
   ; weakened by an amount regulated by *simpl-impact*.
   (defun facilitate (subgoals supergoal &optional (value 1))
   (let ((weight (* *excit-weight* value)))
   (if (listp subgoals)
   (print-si subgoals " facilitate " supergoal)
   (print-si subgoals " facilitates " supergoal)
   )
   (if (atom subgoals) (setq subgoals (list subgoals)))
   (dolist (propn (cons supergoal subgoals))
   (when (not (member propn *all-propositions*))
   (error "The proposition ~s has not been defined.%This explanatory relation
   not entered in the network." propn)
   (return-from facilitate)
   )
   )
   (make-sym-links (cons supergoal subgoals)
   (/ weight (expt (length subgoals) *simpl-impact*))
   )
   (pushnew supergoal *all-supergoals*)
   (setq *all-subgoals* (union subgoals *all-subgoals*))
   (setf (facilitated-by supergoal)
   (union subgoals (facilitated-by supergoal))
   )
   (dolist (subgoal subgoals) (pushnew supergoal (facilitates subgoal)))
   )
   )

   ; INCOMPATIBLE creates incoherence between pairs of goals that cannot both
   ; be achieved. The optional argument VALUE alows the amount which the
   ; goals incohere to be regulated.
   (defun incompatible (goal1 goal2 &optional (value 1))
   (if (not (eq 1 value))
   (print-si goal1 " and " goal2 " are incompatible at value " value)
   (print-si goal1 " and " goal2 " are strongly incompatible at value "
   value)
   )
   (make-sym-link goal1 goal2 (* *inhib-weight* value))
   (pushnew goal2 (contradicts goal1))
   (pushnew goal1 (contradicts goal2))
   (pushnew (list goal1 goal2) *contradictions*)
   )

    

   ; GENERATE-INCOHERENCE creates a weak incoherence between supergoals
   ; whose subgoals incohere. The degree to which the supergoals
   ; incohere is regulated by *weak-incohere-impact*. NOT CURRENTLY USED. PT,
   5-20-93
   (defun generate-incoherence (contradictions)
   (let (supergoals1 supergoals2)
   (print-si "Looking for supergoals with incompatible subgoals...")
   (dolist (pair contradictions)
   (setq supergoals1 (facilitates (car pair))
   supergoals2 (facilitates (second pair))
   )
   (if (and supergoals1 supergoals2)
   (incompatible-set supergoals1 supergoals2 *weak-incohere-impact*)
   )
   )
   )
   )

   ; INCOMPATIBLE-SET creates incoherence between two sets of goals which
   ; cannot be or are difficult to achieve together. The more goals, the
   ; weaker the links. NOT CURRENTLY USED. PT, 5-20-93
   (defun incompatible-set (goalset1 goalset2 &optional (value 1))
   (let ((weight (/ *inhib-weight*
   (/ (+ (length goalset1) (length goalset2)) 2)
   )
   ))
   (if (not (eq 1 value))
   (print-si goalset1 " and " goalset2 " are incompatible at value " value)
   (print-si goalset1 " and " goalset2 " are incompatble")
   )
   (dolist (goal1 goalset1)
   (dolist (goal2 goalset2)
   (make-sym-link goal1 goal2 weight)
   (pushnew (list goal1 goal2) *contradictions*)
   )
   )
   )
   )

    

   ; ANALOGOUS sets up links between 2 analogous supergoals and
   ; between 2 analogous subgoals. NOT CURRENTLY USED. PT, 5-20-93
   ;(defun analogous (subgoals supergoals)
   ; (cond ((and (member (car supergoals) (facilitates (car subgoals)))
   ; (member (second supergoals)(facilitates (second subgoals)))
   ; )
   ; (make-sym-link (car supergoals) (second supergoals);;
   ;
   ; )
   ; (make-sym-link (car subgoals) (second subgoals)
   ; (* *excit-weight* *analogy-impact*)
   ; )
   ; (print-si '" Facilitation of " (car supergoals)
   ; '" by " (car subgoals)
   ; '" is analogous to facilitation of " (second supergoals)
   ; '" by " (second subgoals)
   ; )
   ; )
   ; else:
   ; (t (error "Facilitation not present in analogy."))
   ; )
   ;)

    

    

    
