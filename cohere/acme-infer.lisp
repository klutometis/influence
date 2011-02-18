   ; FILE: acme-infer.lisp
   ; PURPOSE: analogical transfer of propositions
   ; PROGRAMMER: Paul Thagard, adapting Eric Melz's work on Copying with
   Substitution and Generation
   ; CREATED: 5/14/98
   ; UPDATE: 12/1/98. broad-infer fixed to handle emotional analogies.

    

   ; Analogical inference involves transfer of information from a source to a
   ; target. This can be broad or specific:
   ; 1. Broad: What does the source tell you about the target?
   ; - do full CWSG as in Melz, but make transfer more focused
   ; on the source and target, not on *all-propositions*.
   ; 2. More specific: What does the source tell you about particular
   ; aspects of the target, e.g. predicates and/or objects.
   ; BROAD-INFER expands the target as fully as possible using all
   ; source propositions not already mapped to a target proposition.
   ; SPEC-INFER adds to the target only one proposition.
   ; The result of both kinds of inference is that a proposition in
   ; the target is positively linked to the proposition that
   ; was used to generate it. Analogical inference can be both novel
   ; and routine. Novel: a new proposition is constructed in the target.
   ; Routine: an already existing proposition in the target is linked
   ; to its analog in the source.
   ; For system mappings, where the propositions are related as part of
   ; the same causal structure, the links should be much stronger
   ; than ordinary similarities.
   ; Forging new activation links should automatically enable transfer
   ; of valences too, modeling emotional analogies. Inference with
   ; multiple analogies will also be automatically be modeled, since
   ; propositions from different source analogs can all be linked to
   ; a single proposition in the target.

   ; New variables

   (defvar *propn-number* 0)
   (defvar *arg-number* 0)
   (defvar *min-match-activation* .6) ; matches have to be this good
   (defvar *ana-assn* .5) ; analogical association in absence of mapping
   hypothesis
   (defvar *hypotheses-generated* nil) ; new hypotheses generated

    

   ; SPEC-INFER makes possible inference of a single proposition in the
   target
   ; supported by its analog in the source. If the proposition to be inferred
   already
   ; exists, then all SPEC-INFER does is link it to its analog. Otherwise, it
   uses
   ; CWSG to produce a new proposition and link it to its analog.
   ; Here propn-to-infer is a pair of (propn-from-source propn-from-target)

   (defun spec-infer (source target predicate 1-or-more-objects)
   (let ((propn-to-infer (find-propn source target predicate
   1-or-more-objects)))
   (unless propn-to-infer ; if it doesn't exist
   (setf propn-to-infer ; make it by cwsg
   (make-propn-to-infer source target predicate 1-or-more-objects)
   )
   )
   (associate-ana (first propn-to-infer) (second propn-to-infer)) ; link the
   analogs

   (my-print "Making inferential connection: " propn-to-infer)
   )
   )

   ; ASSOCIATE-ANA associates two analogous propositions, the first from the
   source,
   ; the second from the target. The strength of the association is based on
   how
   ; good the mapping is, i.e. on the activation of the unit that represents
   a
   ; mapping between the two propositions.
   (defun associate-ana (sprop tprop)
   (associate sprop tprop (or (get (catname sprop tprop) 'activation)
   *ana-assn*))
   )

   ; FIND-PROPN looks in a target structure to see if there is a proposition
   involving
   ; a given predicate and 1 or more objects. If so, it returns a list
   ; of the the name of the analogous proposition in
   ; the source, if it exists and the proposition name. Otherwise, it returns
   nil.

   (defun find-propn (source target predicate 1-or-more-objects)
   (do ((propns (get target 'propositions) (cdr propns))
   )
   ((null propns) nil) ; return
   ;repeat
   (if (propn-matches (list predicate 1-or-more-objects)
   (get (car propns) 'message)
   )
   (return (list (or (best-match-in-struc source (car propns))
   (find-propn-unmapped source (car propns))
   )
   (car propns)
   )
   )
   )
   )
   )

   ; FIND-PROPN-UNMAPPED handles the case where there is no
   best-match-in-struc
   ; because the proposition is newly created by another analogy.
   ; NOTE: this may not be the best match, but it would rarely matter.
   (defun find-propn-unmapped (source propn)
   (do ((propns (get source 'propositions) (cdr propns)))
   ((null propns) nil)
   ; repeat
   (if (propn-matches (get propn 'message)
   (get (car propns) 'message)
   )
   (return (car propns)) ; found
   )
   )
   )

   ; PROPN-MATCHES determines if two messages have a similar
   ; predicate and at least some of the same arguments.

   (defun propn-matches (mess1 mess2)
   (and (or (equal (car mess1) (car mess2))
   (similar? (car mess1) (car mess2))
   )
   (intersection (second mess1) (second mess2))
   )
   )

   ; BEST-MATCH-IN-STRUC is like BEST-MATCH in acme.lisp, except that it
   ; should make sure, for the sake of multiple analogies, that the match
   found
   ; is from the specified structure. This works with predicates, objects,
   ; and propositions. Using the constructed-matches property, it
   ; checks for matches that have been constructed by generation.

   (defun best-match-in-struc (struc el)
   (let (bm bm2 best-unit other-good-units)
   (setq best-unit (highest-l (constraints-for-structure struc el)
   'activation
   )
   )
   (cond ((null best-unit) nil) ; nothing matches
   ((match-generated struc el) (match-generated struc el))
   (t
   ; else do all of these:
   ; ADD remove matches from other structures - NOT YET implemented
   (if (listp best-unit)
   (setq best-unit (car (setq other-good-units best-unit)))
   )
   (setq bm (other-from-pair el (get best-unit 'concerns)))
   (my-print "Best mapping of " el " in " struc " is " bm ". "
   (get best-unit 'activation)
   )
   (if other-good-units
   (do ((units (cdr other-good-units) (cdr units)))
   ((null units) nil)
   (setq bm (other-from-pair el (get (car units) 'concerns)))
   (my-print " tied with " bm2 ".")
   )
   )
   (if (> (get best-unit 'activation) *min-match-activation*)
   bm ; return best match if it has high enough activation
   ; otherwise return nil
   nil
   )
   ) ; end t
   ) ; end cond
   ) ; end let
   )

   ; MATCH-GENERATED looks for a match that has been previously constructed
   ; by make-propn-to-infer using make-arg

   (defun match-generated (struc ele)
   (cdr (assoc ele (get struc 'generated-matches)))
   )

   ; CONSTRAINTS-FOR-STRUCTURE (struc el) returns a list of all the
   constraint-hyps
   ; for an element - object, predicate, or proposition -
   ; that are relevant to the given structure. This is for multiple
   ; analogies, where different mappings can co-exist.
   (defun constraints-for-structure (struc el)
   (do ((constraints (get el 'constraint-hyps) (cdr constraints))
   (result nil)
   )
   ((null constraints) result)
   (if (constraint-relevant struc (car constraints))
   (push (car constraints) result)
   )
   )
   )

   ; CONSTRAINT RELEVANT returns T if the given constraint appertains to
   ; a given source structure.
   (defun constraint-relevant (struc constraint)
   (member (car (get constraint 'concerns))
   (elements-from-structure struc)
   )
   )

   ; ELEMENTS-FROM-STRUCTURE returns a list of all the predicates, arguments,
   and
   ; propositions from a structure.
   (defun elements-from-structure (struc)
   (do ((propns (get struc 'propositions) (cdr propns))
   (result nil)
   )
   ((null propns) result)
   (setq result
   (union-list (list (car propns)
   (pred-from-propn (car propns))
   )
   (args-from-propn (car propns))
   result

   )
   )
   )
   )

    

   ; MAKE-PROPN-TO-INFER uses information in the source to produce a new
   proposition
   ; in the target to infer. The predicate in the new proposition is the
   ; same as in the source, but the arguments are completed by either
   ; subsitution (using the best match) or generation of a new argument.
   ; The predicate must have a corresponding predicate in the source.
   ; Should there be a similar check for objects?
   ; Returns a pair of (propn-from-source propn-from-target)

   (defun make-propn-to-infer (source target pred object-lst)
   (let ((new-message nil)
   (new-propn (gen-target-propn target pred))
   (source-propn (find-source-propn source pred))
   )
   (if (null source-propn) ; no source proposition found
   (progn (my-print "No source analog found for " pred " and " object-lst)
   nil ; return nothing
   )
   ; else construct the message
   (do ((arguments (reverse (new-args-from-propn source-propn)) (cdr
   arguments))
   (new-arguments nil)
   )
   ((null arguments) ; exit: construct new message
   (setf new-message (list pred new-arguments new-propn))
   (setf (get new-propn 'message) new-message)
   (push new-message (get target 'all)) ; ADD more specific field
   (push new-propn (get target 'propositions))
   ; store match with source:
   (setf (get source 'generated-matches)
   (acons source-propn new-propn (get source 'generated-matches))
   )
   (process-cause new-propn) ; provide input to ECHO if causal
   (list source-propn new-propn) ; return pair
   )
   ; repeat construction of arguments
   (setq new-arguments (push (make-arg source (car arguments))
   new-arguments))
   )
   )
   )
   )

   ; NEW-ARGS-FROM-PROPN is slightly different from the original in ACME
   which
   ; has an additional screening function.

   (defun new-args-from-propn (propn)
   (second (get propn 'message))
   )

   ; FIND-SOURCE-PROPN makes sure that there is a source proposition that can
   ; provide information about the target predicate. The source proposition
   has
   ; to have the same predicate as the predicate to be inferred about.
   ; This takes the first occurrence - ideally it should make more than
   ; one possible inference.

   (defun find-source-propn (source pred)
   (do ((propns (get source 'propositions) (cdr propns)))
   ((null propns) nil); exit - nothing found
   ; repeat
   (if (equal pred (car (get (car propns) 'message)))
   (return (car propns))
   )
   )
   )

   ; MAKE-ARG takes the analogous object if there is one, and generates
   ; a new one ARG-NEW-# if there isn't.
   ; If the argument is a list, as in (CAUSE (CAUSE1 CAUSE2 ...) EFFECT),
   ; need to iterate.

   (defun make-arg (source arg)
   (let ((new-arg nil))
   (if (listp arg) ; argument is a list
   (make-all-args source arg)
   ; else argument is is an object
   (or (best-match-in-struc source arg)
   (match-generated source arg)
   (progn (incf *arg-number*)
   (setf new-arg (concat-2 (concat-2 arg '-new-) *arg-number*))
   (setf (get source 'generated-matches)
   (acons arg new-arg (get source 'generated-matches))
   )
   new-arg
   )
   )
   )
   )
   )

   ; MAKE-ALL-ARGS makes an argument consisting of a list of objects.

   (defun make-all-args (source arg)
   (do ((args (reverse arg) (cdr args))
   (result nil)
   )
   ; exit
   ((null args) result)
   ; repeat:
   (setq result (push (make-arg source (car args)) result))
   )
   )

   ; GEN-TARGET-PROPN generates a new proposition to be part of a target

   (defun gen-target-propn (target pred)
   (let ((new nil))
   (setf new (concat-2 (concat-2 pred '-new- ) *propn-number*))
   (push new (get target 'propositions))
   (proposition new "Hypothesis")
   (incf *propn-number*)
   (my-print "New proposition " new " made for " target)
   (push new *hypotheses-generated*)
   new ; return name of proposition
   )
   )

   ; CONCAT-2 makes a new atom out of two given atoms; cf. CATNAME in
   acme.lisp
   ; CONCAT in acme.lisp combines any number of atoms.
   ;
   (defun concat-2 (atm1 atm2)
   (read-from-string (coerce (append (coerce (princ-to-string atm1) 'list)
   (coerce (princ-to-string atm2) 'list)
   )
   'string
   )
   )
   )

   ; PROCESS-CAUSE takes a newly generated proposition and produces input
   ; to ECHO if appropriate. To be added: other predicates for ECHO and DECO.

   (defun process-cause (propn)
   (cond ((equal (pred-from-propn propn) 'cause)
   (explain (first (new-args-from-propn propn))
   (second (new-args-from-propn propn))
   )
   )
   )
   )

    

   ; MY-SORT alphabetizes
   (defun my-sort (lst) (sort lst #'string-lessp))

   ; BROAD-INFER does full pattern completion on a source, generating
   ; new propositions for every source proposition that lacks an analogous
   ; target proposition. This function is analogous to Melz's TRANSFER.
   ; But it also creates links to make inferences. For each
   ; proposition of the source, it either finds the most analogous
   ; proposition of the target, or else creates a new one.
   ; BUGS 12-1-98: slight redundancy in making of propositions and
   associations.
   ; 3-19-99: need to reverse proposition list to ensure causes get
   ; processed last.

    

   (defun broad-infer (source target)
   (do ((propns (reverse (get source 'propositions)) (cdr propns))
   (message nil)
   (ana-prop nil) ; analogous proposition
   (pairs-to-connect nil)
   )
   ((null propns)
   (my-print source " applied to " target " yields: " )
   (pl target)
   (my-print "Making inferential connections: " pairs-to-connect)
   (associate-pairs pairs-to-connect) ; link the analogs
   )
   ; repeat
   (setf message (get (car propns) 'message))
   (my-print "Transferring " message)
   (setf ana-prop (best-match-in-struc source (car propns)))
   (if ana-prop ; a good match already exists
   (push (list ana-prop (car propns)) pairs-to-connect)
   ; else make a new proposition to infer
   (push (make-propn-to-infer source target (car message) (second message))
   pairs-to-connect
   )
   )
   )
   )

   ; ASSOCIATE-PAIRS takes a list of pairs and establishes a positive
   constraint
   ; between each pair.
   (defun associate-pairs (lst)
   (do ((pairs lst (cdr pairs)))
   ((null pairs) (my-print "Inferences made."))
   (associate-ana (caar pairs) (second (car pairs)))
   )
   )

    

   ; ACTIVATE-PROPOSITIONS initializes all propositions for further
   inference.

   (defun activate-propositions ()
   (mapcar #'activate-prop *all-propositions*)
   )

   ; ACTIVATE-PROP

   (defun activate-prop (prop)
   (put prop 'activation *init-activ*)
   (put prop 'valence *init-activ*)
   )
