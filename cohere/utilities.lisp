   ; FILE : utilities.lisp
   ; PURPOSE: utilities for COHERE
   ; NOTE : Based on the utilities given for DECO.

    

    

    

   ; MY-PRINT nicely prints a collection of arguments.
   (defun my-print (&rest arguments)
   (prog (args)
   (setq args arguments)
   loop
   (cond ( (null args) (terpri) (return t)))
   (princ (car args) )
   (setq args (cdr args))
   (go loop)
   )
   )

    

   ; PRINT-RUN prints out the emperiment and test names, and the units
   ; that have not reached assymptote.
   (defun print-run (&optional *verbose*)
   (cond (*silent-run?*)
   (t (my-print '"Test: " *testnum*
   '" Total times: " *total-times*
   )
   ; (unix "date")
   (my-print *problem*) ; description
   (my-print '"Units not yet reached asymptote: "
   (length (set-difference *all-units*
   *asymptoted-units*
   )
   )
   )
   (print-propns)
   )
   )
   )

    

   ; PRINT-PROPNS displays the activation of each unit.
   (defun print-propns ()
   (mapcar #'print-activation
   *all-units*
   )
   )

   ; PRINT-ACTIVATION prints a unit, the proposition it represents, and its
   ; activation.
   (defun print-activation (unit)
   (my-print unit '" @ " *total-times* '" is " (get unit 'activation))
   ; (my-print '" " (prop unit))
   )

   ; PRINT-VALUES prints the parameters of the network.
   (defun print-values ()
   (my-print '"Current network parameters:")
   (my-print '" Decay : " *decay-amount*)
   (my-print '" Excitation: : " *excit-weight*)
   (my-print '" Inhibition: : " *inhib-weight*)
   (my-print '" Data excitation: : " *data-excit*)
   (my-print '" Total number of units : " (length *all-units*))
   (my-print '" Total number of symmetric links: " (/ *total-links* 2))
   (my-print '" Total number of links : " *total-links*)
   (my-print '" Threshold for output from units: " *output-threshold*)
   (my-print '" (default) Minimum activation : " *min-activation*)
   (my-print '" (default) Maximum activation : " *max-activation*)
   (my-print '" Asymptote criterion : " *asymptote*)
   (my-print '" Priority goal excitation : " *goal-excit*)

   )

   ;

   (defun act (unit)
   "Function to determine the activation of a unit."
   (get unit 'activation)
   )

   (defun debug-print (message)
   (if (null *debug*) nil (print message)))

    

   ;**************************************************************************
   ; The following functions have been imported from versions of ECHO

   (defun print-si (&rest arguments)
   (unless *silent-run?* (apply #'my-print arguments))
   )
   (defun silent () (setq *silent-run?* t) (defun debug-run () nil))

   ;

   ; PPLIST pretty-prints out a property list.
   (defun pplist (atom)
   (my-print '" ")
   (if (boundp atom)
   (my-print '"Property list of " atom " (value = " (eval atom) ")")
   (my-print '"Property list of " atom)
   )
   (do ((lst (plist atom) (cddr lst)))
   ((null lst) t)
   (my-print (car lst) '": " (cadr lst))
   )
   )

   (defun pl (atm) (pplist atm))

   ; NODE-TYPE is a function to check to see if the node is an action, goal
   or subgoal
   (defun node-type (atom)
   (get atom 'type))

   ;PUT-PROP
   (defun put-prop (atom value property)
   (setf (get atom property) value))

   ; ******************************************************
   ; Functions for changing parameters.
   ; ******************************************************
   ; GROSS-ON and GROSS-OFF turn on and off the use of the Grossberg
   ; updating rule.

   (defun gross-on ()
   (if *grossberg?* (or *silent-run?* (my-print '"Already using Grossberg
   rule."))
   ; else:
   (and (setq *grossberg?* t)
   (or *silent-run?* (my-print '"Now using Grossberg updating rule."))
   )
   )
   )

   (defun gross-off ()
   (if (null *grossberg?*)
   (or *silent-run?* (my-print "Already using Rumelhart and McClelland
   updating rule."))
   ; else:
   (or (setq *grossberg?* nil)
   (or *silent-run?* (my-print '"Now using Rumelhart and McClelland updating
   rule."))
   )
   )
   )

   (defun tversky-on ()
   (if *tversky?* (or *silent-run?* (my-print "Already using altered negative
   activation rule."))
   (and (setq *tversky?* t)
   (or *silent-run?* (my-print "Now using altered negative activation
   rule."))
   )
   )
   )

   (defun tversky-off ()
   (if (null *tversky?*) (or *silent-run?* (my-print "Altered rule is not in
   use."))
   (or (setq *tversky?* nil)
   (or *silent-run?* (my-print "No longer using altered negative activation
   rule."))
   )
   )
   )

   ; ****************************************************************
   ; WTP determines when to print system information while running
   ; the net. At every timestep on the list, info is presented.
   ; The run stops with the last timestep on the list.

   (defun wtp (lst)
   (setq *when-to-print* lst)
   (my-print '"Printing results at cycles: " lst)
   )
   ; ***************************************************
   ; STOP says to stop network if it has settled.

   (defun stop ()
   (setq *stop-settled?* t)
   )

   ; UNSTOP

   (defun unstop ()
   (setq *stop-settled?* nil)
   )

   ; ***************************************************

   ; INHIB
   (defun inhib (num)
   (declare (type (float) num))
   (cond ((equal *silent-run?* t))
   ((= num *inhib-weight*)
   (my-print '"Inhibition unchanged at " num)
   )
   ((> num *inhib-weight*)
   (my-print '"Inhibition weakened from " *inhib-weight* '" to " num)
   )
   (t (my-print '"Inhibition intensified from " *inhib-weight* '" to " num))
   )
   (setq *inhib-weight* num)
   )
   ; DECAY
   (defun decay (num)
   (declare (type (float) num))
   (cond ( (equal *silent-run?* t))
   ( (= num *decay-amount*)
   (my-print '"Decay unchanged at " num)
   )
   ( (> num *decay-amount*)
   (my-print '"Decay rate increased from " *decay-amount* '" to " num)
   )
   ( t (my-print '"Decay rate decreased from " *decay-amount* '" to " num))
   )
   (setq *decay-amount* num)
   )

   ; EXCIT
   (defun excit (num)
   (declare (type (float) num))
   (cond ( (equal *silent-run?* t))
   ( (= num *excit-weight*)
   (my-print '"Excitation unchanged at " num)
   )
   ( (> num *excit-weight*)
   (my-print '"Excitation increased from " *excit-weight* '" to " num)
   )
   (t (my-print '"Excitation decreased from " *excit-weight* '" to " num))
   )
   (setq *excit-weight* num)
   )

   ; DATA-EXCIT
   (defun data-excit (num)
   (declare (type (float) num))
   (cond ( (equal *silent-run?* t))
   ( (= num *data-excit*)
   (my-print '"Data excitation unchanged at " num)
   )
   ( (> num *data-excit*)
   (my-print '"Data excitation increased from " *data-excit* '" to " num)
   )
   (t (my-print '"Data excitation decreased from " *data-excit* '" to " num))
   )
   (setq *data-excit* num)
   )

   ; SIMPLE
   (defun simple (num)
   (declare (type (float) num))
   (if (> num *simpl-impact*)
   (my-print '"Simplicity impact increased from " *simpl-impact* '" to " num)
   ; else
   (my-print '"Simplicity impact decreased from " *simpl-impact* '" to " num)
   )
   (setq *simpl-impact* num)
   )

   ; OUTPUT

   (defun output (num)
   (declare (type (float) num))
   (cond ( (equal *silent-run?* t))
   ( (= num *output-threshold*)
   (my-print '"Output threshold unchanged at " num)
   )
   ( (> num *output-threshold*)
   (my-print '"Output theshold increased from " *output-threshold* '" to "
   num)
   )
   ( t (my-print '"Output threshold decreased from " *output-threshold* '" to
   " num))
   )
   (setq *output-threshold* num)
   )

   ; CONS-IF-NEW adds an element if it is not already there.

   (defun cons-if-new (el lst)
   (if (member el lst :test #'equal) lst
   ; else
   (cons el lst)
   )
   )

    

    

   (defmacro plist (atm) `(symbol-plist ,atm))

    

   (defmacro not-equal (atm1 atm2) `(not (equal ,atm1 ,atm2)))

   (defmacro my-max (num1 num2) `(max ,num1 ,num2))

   (defmacro subset (l1 l2) `(subsetp ,l1 ,l2))

    

    

   (defun silent () (setq *silent-run?* t) )
   (defun unsilent () (setq *silent-run?* nil))

   (defun justify (spaces atm)
   "Justifies atom ATM in a string of SPACES characters."
   (let (outstr)
   (setq outstr (coerce (princ-to-string atm) 'list))
   (do ((len (length outstr) (1+ len)))
   ((eq len spaces) (coerce outstr 'string))
   (setq outstr (cons #\Space outstr))
   )
   )
   )

    

   (defun swap (lis pos1 pos2)
   "Exchanges elements POS1 and POS2 of list LIS."
   (do ((pos 0 (1+ pos))
   (elt1 (elt lis pos1))
   (elt2 (elt lis pos2))
   (retlis nil)
   )
   ((eq pos (length lis)) (reverse retlis))
   (setq retlis (cons (cond ((eq pos pos1) elt2)
   ((eq pos pos2) elt1)
   (t (elt lis pos))
   )
   retlis
   )
   )
   )
   )

   ;; BEGIN newsym functions (similar to gensym)
   (defvar *NEWSYM-PREFIX* 'c)

   (defun newsym (symb)
   "Given a symbol, get it's counter and create a new symbol consisting
   of the symbol concat'ed with its number. If symbol is nil, use
   the current value of *NEWSYM-PREFIX*"
   (cond ((symbolp symb)
   (if (null symb) (setq symb *NEWSYM-PREFIX*))
   (let (count)
   (if (null (get symb '*newsym-counter*))
   (setf (get symb '*newsym-counter*) 0))
   (setf (get symb '*newsym-counter*)
   (1+ (setq count (get symb '*newsym-counter*))))
   (concat symb count)))
   (t (princ "Error: non-symbol arg to newsym ")
   (princ symb))))

   ; **********************************************************
   ; ATOM-BEGINS checks to see whether an atom begins with a
   ; given character.

   (defun atom-begins (atm char)
   (eq (aref (coerce-string atm) 0) char)
   )

   ; COERCE-STRING turns an atom or number into a string

   (defun coerce-string (atm)
   (coerce (coerce (princ-to-string atm) 'list) 'string)
   )

   ; ATOM-INCLUDES checks to see whether an atom includes a given
   ; character.

   (defun atom-includes (atm char)
   (prog (str index)
   (setq str (symbol-name atm))
   (setq index 0)
   loop
   (if (> (+ 1 index) (length str)) (return nil))
   (if (equal (aref str index) char) (return t))
   (setq index (+ 1 index))
   (go loop)
   )
   )

    

    

   ; **************************************************************
   ; UNION-LIST takes any number of arguments and returns the
   ; union of all of them.

   (defun union-list (&rest arguments) ; takes any number of arguments
   (remove-duplicates (apply 'append arguments))
   )
   ; ********************************************************
   ; UNION-MAP takes the union of all members of a list of lists,
   ; where the list of lists arises from mapcarring a function.
   ; e.g. union-map 'cdr '( (a b) '( 1 2 a)) = (b 2 a)
   (defun union-map (fn lst)
   (apply 'union-list (mapcar fn lst))
   )
   ;**********************************************************
   ; INTERSECTION-LIST takes any number of arguments and returns
   ; their intersection.

   (defun intersection-list (&rest arguments)
   (prog (args result)
   (setq args arguments
   result nil
   )
   loop
   (cond ( (null args) (return result)))
   (setq result (intersection (car args) result))
   (setq args (cdr args))
   (go loop)
   )
   )

   ; ***********************************************************
   ; REMOVE-LIST removes all members of list1 from list2

   (defun remove-list (lst1 lst2)
   (prog (ls result)
   (setq ls lst1)
   (setq result lst2)
   loop
   (cond ( (null ls) (return result)))
   (setq result (remove (car ls) result))
   (setq ls (cdr ls))
   (go loop)
   )
   )

   ; *********************************************************
   ;
   ; HIGHEST (list property) returns that member of the list
   ; which has the highest value on its property list of the
   ; given property. HIGHEST-L does the same, but returns a list
   ; when there are ties.

   (defun highest (list property)
   (do ((lst (cdr list) (cdr lst))
   (high (car list))
   (highval (get (car list) property))
   (val nil)
   )
   ((null lst) high)
   (cond ((> (setq val (get (car lst) property)) highval)
   (setq high (car lst)) (setq highval val)
   )
   )
   )
   )

   (defun highest-l (list prop)
   (if (null list) nil
   ; else
   (do ((lst list (cdr lst))
   (high nil)
   (values (mapcar #'(lambda (el) (get el prop)) list))
   )
   ((null lst) (if (listp high) high (car high)))
   (if (equal (get (car lst) prop) (apply 'max values))
   (setq high (cons (car lst) high))
   )
   )
   )
   )

   ; *************************************************************
   ; PRINT-PLIST-S prints out the plists of all members of a list

   (defun print-plist-s (lst)
   (mapcar 'print-plist lst)
   )

   (defun pls (lst) (mapcar 'print-plist lst))

   ;
   **************************************************************************
   ; print-plist pretty-prints out a property list.

   (defun print-plist (atom)
   (my-print '" ")
   (if (boundp atom)
   (my-print '"Property list of " atom " (value = " (eval atom) ")")
   (my-print '"Property list of " atom)
   )
   (do ((lst (plist atom) (cddr lst)))
   ((null lst) t)
   (my-print (car lst) '": " (cadr lst))
   )
   )

   (defun pl (atm) (print-plist atm))

    

   ; ************************************************************************
   ; MIN-MAX returns a value between low and high.

   (defun min-max (low high num) (min (max low num) high))

    

    

   ; REMOVE-NIL-DUP

   (defun remove-nil-dup (lst)
   (remove-duplicates (remove nil lst))
   )

   ; FLATTEN is the typical flatten function.

   (defun flatten (lis)
   "Removes nestings from a list."
   (cond ((atom lis) lis)
   ((listp (car lis))
   (append (flatten (car lis)) (flatten (cdr lis)))
   )
   (t (append (list (car lis)) (flatten (cdr lis))))
   )
   )
