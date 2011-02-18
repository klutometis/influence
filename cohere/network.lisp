   ; FILE: network.lisp
   ; PURPOSE: network operations for COHERE
   ; UPDATED: 6-23-2000

    

    

   ;*********************************************************************************
   ;; GET/PUT MACROS

   (defmacro activation (unit) `(get ,unit 'activation))
   (defmacro prop (unit) `(get ,unit 'prop))
   (defmacro original-activation (unit) `(get ,unit 'original-activation))
   (defmacro facilitates (unit) `(get ,unit 'facilitates))
   (defmacro facilitated-by (unit) `(get ,unit 'facilitated-by))
   (defmacro contradicts (unit) `(get ,unit 'contradicts))
   (defmacro new-activation (unit) `(get ,unit 'new-activation))
   (defmacro links-from (unit) `(get ,unit 'links-from))
   (defmacro plist (atm) `(symbol-plist ,atm))

    

   ; ****************************************
   ; * FUNCTIONS FOR CLEARING THE NETWORK *

   ; CLEAR-PROPS clears a unit's property list.
   (defun clear-props (atm)
   (setf (symbol-plist atm) nil)
   )

   ; ****************************************

   ; CLEAR-NET starts the beginning of a run by cleaning up the
   ; results of past runs.

    

   (defun clear-net ()
   (unless *silent-run?* (my-print '"Clearing net."))
   (setq *stop-run?* nil)
   ; clear all property lists:
   (clear-props 'special)
   (mapcar #'clear-props *all-units*)
   (mapcar #'clear-props *all-valence-units*)
   (unless *silent-run?* (my-print "Units cleared."))
   (setq *total-times* 1)
   (setq *total-links* 0)
   (setq *all-units* nil)
   (setq *all-data* nil)
   (setq *asymptoted-units* nil)
   (setq *all-propositions* nil)
   (setq *all-supergoals* nil)
   (setq *all-subgoals* nil)
   (setq *all-basic-goals* nil)
   (setq *contradictions* nil)
   (setq *settled?* nil)
   (mapcar #'clear-struc *all-structures*) ; for ACME
   (setq *all-units* nil)
   (setq *all-valence-units* nil) ; for HOTCO
   (setq *evaluation-units* nil) ; for HOTCO 2
   (setq *all-xs* nil *all-xsit* nil) ; for expln. schemas
   (unless *silent-run?* (my-print "Network cleared."))
   )

    

   ; ********************************************
   ; * FUNCTIONS FOR CREATING UNITS AND LINKS *
   ; ********************************************

   ; NOTE-UNIT sets the global variables indicating a unit has been added.
   (defun note-unit (unit)
   (setf (activation unit) *init-activ*)
   (pushnew unit *all-units*)
   )

   ; MAKE-SYM-LINKS creates symmetric links between all of the members of a
   set
   ; of units.
   (defun make-sym-links (list-of-units weight)
   (do ((units list-of-units (cdr units)))
   ((null units) nil)
   (dolist (unit (cdr units))
   (make-sym-link (car units) unit weight)
   )
   )
   )

   ; MAKE-SYM-LINK sets up a symmetric link between units.
   ; Important to make no links from unit to itself, and don't clobber
   ; excitatory links with inhibitory ones.
   ; Excitatory links sum, but inhibitory don't.
   (defun make-sym-link (unit1 unit2 weight)
   (cond ( (and (not (eq unit1 unit2))
   (>= (weight-of-link-between unit1 unit2) 0)
   )
   ; then:
   (make-link unit1 unit2 weight)
   (make-link unit2 unit1 weight)
   )
   )
   )

    

   ; MAKE-LINK sets up a 1-way link. It adds the weight to whatever
   ; weight (initially 0) was on the link.
   (defun make-link (unit1 unit2 weight)
   (let (old)
   (cond ((setq old (assoc unit2 (links-from unit1)))
   (nsubstitute (cons unit2 (+ weight (cdr old))) old
   (links-from unit1)
   )
   )
   (t
   (setf (links-from unit1)
   (acons unit2 weight (links-from unit1)))
   (setq *total-links* (1+ *total-links*))
   )
   )
   )
   )

    

    

   ; WEIGHT-OF-LINK-BETWEEN finds the value of a link between two units.
   (defun weight-of-link-between (unit1 unit2)
   (or (cdr (assoc unit2 (links-from unit1))) 0)
   )

   (defun WEIGHT_OF_LINK_BETWEEN (unit1 unit2)
   (weight-of-link-between unit1 unit2))

   ; ***************************************
   ; * FUNCTIONS FOR RUNNING THE NETWORK *
   ; ***************************************

   ; RUN-EXP is the top-level function. It is called from the data file,
   ; usually as the last command. It sets the activation of the special unit,
   ; settles the network (by calling RUN-HYP-NET), and prints out the
   results.
   (defun run-exp ()
   (print-si "Running COHERE connectionist algorithm.")
   (setf (activation 'special) *special-activation*)
   (my-print " *****")
   (run-hyp-net)
   (unless *silent-run?*
   (print-propns)
   )
   )

   ; RUN-HYP-NET is the main loop, used for settling the network. For each
   ; cycle it updates the activations of the units, checks for assymptoted
   ; units, and prints out useful information on it's status.
   (defun run-hyp-net ()
   ; (unless *silent-run?* (print-values))
   (setq *testnum* (gensym "test"))
   (do ((timestep 1 (+ 1 timestep))
   (units *all-units*)
   (old-asymptoted nil)
   )
   ( (or *stop-run?*
   (and *stop-settled?* *settled?*) ; network has settled
   (> timestep *max-times*)
   )
   (and (print-run 'verbose)
   (if *silent-run?* (my-print '"Run finished. "))
   )
   )
   (if *grossberg?* ; use Grossberg's updating rule.
   (mapcar #'update-unit-activn-gross units)
   ; else use Rumelhart & McClelland rule:
   (mapcar #'update-unit-activn
   (set-difference units *evaluation-units*)
   )
   ; note 6-23-2000 - remove evaluation units to prevent
   ; duplicate their updating
   )
   (update-valences) ; used by HOTCO - does evaluation units too
   (setq *settled?* t) ; has network reached asymptote?
   ; this turns nil if unit not asymptoted.
   (setq old-asymptoted *asymptoted-units*)
   (setq *asymptoted-units* nil)
   (mapcar #'fix-activation units)
   (fix-valences) ; used by HOTCO
   (unless *silent-run?*
   (mapcar #'announce-asymptote
   (set-difference *asymptoted-units* old-asymptoted)
   )
   )
   (if (and *stop-settled?* *settled?* (not *silent-run?*))
   (my-print '"Network has settled by cycle " *total-times* ".")
   )
   ;(if (member timestep *when-to-print*) (print-propns))

   ; for graphics:
   (if *use-actgraph* (show-act))
   ; (if *trace-list* (update-trace)) ;?????????

   (setq *total-times* (+ *total-times* 1))

   )

   )

    

    

   ; UPDATE-UNIT-ACTIVN updates the activation of a unit based on the
   ; links it has.
   (defun update-unit-activn (unit)
   (declare (ftype (function (&rest float) float) min max + * -)
   (ftype (function (float float) symbol) >))
   (let ((net-input-value (net-input unit)))
   (declare (type (float) net-input-value))
   (setf (new-activation unit)
   (min *max-activation*
   (max *min-activation*
   (+ (* (activation unit) (- 1.0 *decay-amount*))
   (if (> net-input-value 0.0)
   (* net-input-value
   (- *max-activation* (activation unit))
   )
   ; else:
   (* net-input-value
   (- (activation unit) *min-activation*)
   )
   )
   )
   )
   )
   )
   )
   )

    

   ; NET-INPUT is the weighted sum of output from all input units.
   (defun net-input (unit)
   (declare (ftype (function (&rest float) float) max + *))
   (do ((links (links-from unit) (cdr links))
   (result 0.0)
   )
   ((null links) result)
   (declare (type (float) result))
   (setq result (+ (* (float (cdar links))
   (max *output-threshold* (activation (caar links)))
   )
   result
   )
   )
   )
   )

    

   ; FIX-ACTIVATION records the new activation and notes if the unit
   ; has reached asymptote.
   (defun fix-activation (unit)
   (cond ((and *stop-settled?*
   (< (abs (- (new-activation unit)
   (activation unit)
   )
   )
   *asymptote*
   )
   (>= *total-times* *min-settle*)
   )
   (setq *asymptoted-units* (cons unit *asymptoted-units*))
   )
   (t (setq *settled?* nil))
   )
   ; (my-print "Unit: " unit ", difference: " (- (new-activation unit)
   ; (activation unit)))
   (setf (activation unit) (new-activation unit))
   )

    

   ; ANNOUNCE-ASYMPTOTE informs the user of any units recently asymptoted.
   (defun announce-asymptote (unit)
   (my-print "Unit " unit " reached asymptote at cycle "
   *total-times* " with activation "
   (new-activation unit) "."
   )

   )

   ; UPDATE-UNIT-ACTIVN-GROSS updates the activation of a unit based on the
   ; links it has, using Grossberg's rule that treats excitation and
   ; inhibition separately.
   ; Uses global variables *current-excit* and *current-inhib*
   (defun update-unit-activn-gross (unit)
   (declare (ftype (function (&rest float) float) + - * min max))
   ; calculate excitation and inhibition.
   (if *tversky?*
   (excit-and-inhib-tversky unit)
   (excit-and-inhib unit)
   )
   (setf (new-activation unit)
   (min *max-activation*
   (max *min-activation*
   (+ (* (activation unit) (- 1.0 *decay-amount*))
   (* (- *max-activation* (activation unit)) *current-excit*)
   (* (- (activation unit) *min-activation*) *current-inhib*)
   )
   )
   )
   )
   )

   ; EXCIT-AND-INHIB is just like net-input, except that it keeps track
   ; of excitation and inhibition separately. EXCIT-AND-INHIB-TVERSKY is
   ; identical except that units with negative activation can pull down
   ; their excitatory neighbors.
   (defun excit-and-inhib (unit)
   (declare (ftype (function (&rest float) float) max + *)
   (ftype (function (float float) symbol) >))
   (do ((excit 0.0) (inhib 0.0) (wt 0.0) (activn 0.0)
   (links (links-from unit) (cdr links))
   )
   ((null links)
   (setq *current-excit* excit)
   (setq *current-inhib* inhib)
   )
   (declare (type (float) excit inhib wt activn))
   (setq wt (float (cdar links)))
   (setq activn (max *output-threshold* (activation (caar links))))
   (if (> wt 0.0)
   (setq excit (+ excit (* wt activn)))
   ; else wt is inhibitory:
   (setq inhib (+ inhib (* wt activn)))
   )
   )
   )

   (defun excit-and-inhib-tversky (unit)
   (declare (ftype (function (&rest float) float) max + *)
   (ftype (function (float float) symbol) >))
   (do ((excit 0.0) (inhib 0.0) (wt 0.0)
   (links (links-from unit) (cdr links))
   )
   ((null links)
   (setq *current-excit* excit)
   (setq *current-inhib* inhib)
   )
   (declare (type (float) excit inhib wt))
   (setq wt (float (cdar links)))
   (if (> wt 0)
   (setq excit (+ excit (* wt (activation (caar links)))))
   ; else wt is inhibitory:
   (setq inhib (+ inhib (* wt (max *output-threshold*
   (activation (caar links))
   ))))
   )
   )
   )

    

    

   ; these will be resolved during compilation

    

    

    

   (defun devowel (name)
   (coerce
   (do ((tname (coerce (princ-to-string name) 'list) tname)
   (vowels '(#\A #\E #\I #\O #\U) (cdr vowels))
   (cvowel '#\A (car vowels))
   )
   ((null cvowel) tname)
   (setq tname (remove cvowel tname :test #'equal))
   )
   'string
   )
   )

    

   (defun rep (value count)
   "Make a list consisting of COUNT elements of VALUE."
   (make-sequence 'list count :initial-element value)
   )
