   ; FILE: acme.lisp
   ; PURPOSE: analogical constraint mapping
   ; PROGRAMMER: Paul Thagard
   ; CREATED: 5-18-87
   ; UPDATED: 6-12-88
   ; UPDATED: 6-21-88 - objects now taken care of in make-obj-unit.
   ; make-obj-units rewritten as a do loop.
   ; UPDATED: 7-23-88 - Improvements to query mechanism
   ; UPDATED: 8-12-88 - check-importance added and made option for constraint
   ; map
   ; UPDATED: 5-95, to join COHERE
   ; UPDATED: 11-98, fixed sem-similarity

    

    

   ; ******************************
   ; DATA STRUCTURES AND ABSTRACTION
   ; *******************************
   ; MAKE-STRUC creates a structure with a name, type, and an unlimited
   ; number of fields. E.g. a problem will be:
   ; Name of structure:
   ; structure type
   ; start conditions
   ; goals
   ; constraints (optional)
   ; Each of some-fields has the structure:
   ; (property (list of propositions))

   ; Modification to make-struc (GHN, 6/23/88):
   ; If the flag use-selection-list? (nil by default) is non-nil, it will not
   ; make the structure unless it is listed in the global variable
   selection-list.
   ; This allows ARCS routines to select certain structures from the entire
   data-
   ; base which will be used on a given run.

   (defun make-struc (name type &rest some-fields)
   (if (and *use-selection-list?*
   (not (member name *selection-list*))
   )
   (return-from make-struc
   (unless *silent-run?* (my-print "Structure ignored: " name))
   )
   )
   (setq *all-structures* (cons name *all-structures*))
   (put name 'data-type type)
   (do ((fields some-fields (cdr fields)))
   ((null fields) (unless *silent-run?* (my-print '"Structure made: " name)))
   (put name 'fields (cons (caar fields) (get name 'fields)))
   (put name (caar fields) (second (car fields)))
   ; set up propositions:
   (make-propns name (caar fields) (second (car fields)))
   )
   )
   ; ******************************
   ; ADD-STRUC adds to an existing structure:

   (defun add-struc (name field msgs)
   (put name field (union msgs (get name field)))
   (make-propns name field msgs)
   )
   ; ******************************
   ; CLEAR-STRUC removes from memory an existing structure.

   (defun clear-struc (name)
   (remprop name 'constraint-hyps) ; if this hasn't been removed
   (do ((fields (get name 'fields) (cdr fields)))
   ((null fields) nil)
   (remprop name (car fields))
   )
   (remprop name 'fields)
   (remprop name 'data-type)
   (mapcar #'clear-propn (get name 'propositions))
   (remprop name 'propositions)
   (setq *all-structures* (remove name *all-structures*))
   (unless *silent-run?* (my-print '"Structure cleared: " name))
   )

   (defun clear-propn (propn)
   (let ((struc (caar (get propn 'belongs-to)))
   (pred (pred-from-propn propn))
   )
   (remprop propn 'belongs-to)
   (remprop propn 'constraint-hyps)
   (setq *all-propositions* (remove propn *all-propositions*))
   (put pred 'from-propns (remove propn (get pred 'from-propns)))
   (if (put pred 'belongs-to (remove struc (get pred 'belongs-to)))
   nil (clear-pred pred)
   )
   )
   (remprop propn 'message)
   )

   (defun clear-pred (pred)
   (remprop pred 'constraint-hyps)
   (remprop pred 'from-propns)
   (remprop pred 'belongs-to)
   (setq *all-preds* (remove pred *all-preds*))
   )

   ; ******************************

   ; Convention: propositions are names like P1. Messages are
   ; lists: (predicate (arguments) proposition-name)

   ; PROPNS-FROM-STRUC
   (defun propns-from-struc (struc)
   (get struc 'propositions)
   )

   ; GET-PROPN-NAME
   (defun get-propn-name (message)
   (car (last message))
   )
   ; GET-PRED
   (defun get-pred (message)
   (car message)
   )
   ; PRED-FROM-PROPN
   (defun pred-from-propn (propn)
   (get-pred (get propn 'message))
   )
   ; GET-ARGS
   (defun get-args (message)
   (mapcar #'remove-hypothet (second message))
   )
   ; REMOVE-HYPOTHET
   (defun remove-hypothet (arg)
   (if (atom arg) arg (car arg))
   )
   ; ARGS-FROM-PROPN
   (defun args-from-propn (propn)
   (get-args (get propn 'message))
   )
   ; GET-TR-VAL
   (defun get-tr-val (message)
   (third message)
   )
   ; TV-FROM-PROPN
   (defun tv-from-propn (propn)
   (get-tr-val (get propn 'message))
   )

   ; *****************************
   ; CONC-FROM-STRUC lists all the concepts in a structure.

   (defun conc-from-struc (struc)
   (remove-duplicates (mapcar #'pred-from-propn (get struc 'propositions)))
   )

   ; OBJ-FROM-STRUC

   (defun obj-from-struc (struc)
   (union-map #'args-from-propn (get struc 'propositions))
   )

   ; STRUC-FROM-PROPN returns the structure indexed by proposition number

   (defun struc-from-propn (propn)
   (caar (get propn 'belongs-to))
   )
   ; *****************************
   ; MAKE-PROPNS sets up propositions which have the structure:
   ; Name of proposition:
   ; message
   ; belongs-to: ((structure field) ...)

   (defun make-propns (struc field lst-of-messages)
   (do ((msgs lst-of-messages (cdr msgs))
   (result nil)
   (propn nil)
   )
   ((null msgs) result)
   (setq propn (get-propn-name (car msgs)))
   (put propn 'message (car msgs))
   (put propn 'belongs-to
   (cons-if-new (list struc field) (get propn 'belongs-to))
   )
   (setq *all-propositions* (cons-if-new propn *all-propositions*))
   (put struc 'propositions
   (cons-if-new propn (get struc 'propositions))
   )
   (put (get-pred (car msgs)) 'belongs-to
   (cons-if-new struc (get (get-pred (car msgs)) 'belongs-to))
   )
   (put (get-pred (car msgs)) 'from-propns
   (cons-if-new propn (get (get-pred (car msgs)) 'from-propns)
   )
   )
   (setq *all-preds* (cons-if-new (get-pred (car msgs)) *all-preds*))
   )
   )
   ; ******************************
   ; MCON (formerly MAKE-CONCEPT-A) is a bit different from make-concept in
   PI.
   ; As for structures, fields are: ((field-name value) ...)
   ; Typical structure will have some of:
   ; Concept name:
   ; 1. data-type: concept
   ; 2. superordinates: lst
   ; 3. subordinates: lst
   ; 4. part-of: lst
   ; 5. sub-parts: lst
   ; 6. decomp: structured list, representing semantic decomposition
   ; (maybe this should just be *synonyms*?)
   ; 7. rules: lst of rule-names
   ; Add to this: antonyms. Use when propositions are negated.

   ; There is some extra garbage around this stuff to provide a switch
   between
   ; the old version (symmetric-concepts) and the new.

   (if *symmetric-concepts*
   (defun mcon (name fields &optional syntax)
   (put name 'data-type 'concept)
   (put name 'explicit t)
   (setq *all-concepts* (cons-if-new name *all-concepts*))
   (do ((flds fields (cdr flds)))
   ((null flds) (unless *silent-run?* (my-print '"Concept made: " name)))
   (put name
   (caar flds)
   (union (get name (caar flds)) (second (car flds)))
   )
   (note-features name (caar flds) (second (car flds)))
   )
   (if syntax (put name 'syntax syntax))
   )
   (defun mcon (name fields &optional syntax)
   (put name 'data-type 'concept)
   (put name 'explicit t)
   (setq *all-concepts* (cons-if-new name *all-concepts*))
   (do ((flds fields (cdr flds)))
   ((null flds) (unless *silent-run?* (my-print '"Concept made: " name)))
   (put name
   (caar flds)
   (union (get name (caar flds)) (second (car flds)))
   )
   (cond ((eq (caar flds) 'tenses)
   (do ((feats (second (car flds)) (cdr feats)))
   ((null feats) nil)
   (put (car feats) 'root-tense (list name))
   )
   )
   ((eq (caar flds) 'plural)
   (do ((feats (second (car flds)) (cdr feats)))
   ((null feats) nil)
   (put (car feats) 'singular (list name))
   )
   )
   )
   )
   (if syntax (put name 'syntax syntax))
   )
   )

   ; ******************************
   ; NOTE-FEATURES sets up associations from feature to concepts

   (defun note-features (conc field lst)
   (unless (eq field 'rules)
   (associate-conc conc field lst)
   )
   (if (and (eq field 'rules)
   (eq *feature-selection* 'loose)
   )
   (associate-conc conc 'rules (concs-from-rules lst))
   )
   )

   ; ******************************
   ; ASSOCIATE-CONC puts the current concept into the appropriate
   ; field of each feature.

   (defun associate-conc (conc field feats)
   (let (thisfield)
   (cond
   ((equal field 'synonyms) (setq thisfield 'synonyms))
   ((equal field 'antonyms) (setq thisfield 'antonyms))
   ((equal field 'superordinates) (setq thisfield 'subordinates))
   ((equal field 'subordinates) (setq thisfield 'superordinates))
   ((equal field 'sub-parts) (setq thisfield 'part-of))
   ((equal field 'part-of) (setq thisfield 'subparts))
   ((equal field 'tenses) (setq thisfield 'root-tense))
   ((equal field 'plural) (setq thisfield 'singular))
   ((equal field 'variations) (setq thisfield 'root-conc))
   )
   (do ((lst feats (cdr lst)))
   ((null lst) 'done)
   (put (car lst) thisfield
   (cons-if-new conc (get (car lst) thisfield))
   )
   (setq *all-concepts* (cons-if-new (car lst) *all-concepts*))
   )
   )
   )
   ; *************************
   ; CONSTRAINT-MAP uses a constraint network to do analogical
   ; mapping between two problems.

   (defun constraint-map (struc1 struc2)
   (unless *silent-run?* (my-print '"Constructing analogical mapping between
   "
   struc1 '" and " struc2
   )
   )

   ; note objects and concepts for sake of clear-net:
   ; the objects are now taken care of in make-obj-unit - GHN 6/21/88
   (setq *all-preds* (union (conc-from-struc struc1)
   (conc-from-struc struc2)
   )
   )

   ; note: *all-propositions* set up by make-struc

   ; set up the network:
   (make-constraint-net struc1 struc2)
   (if *use-auto-prag?* (check-importance struc1))
   )

   ; **************************

   ; PROPNS-FROM-MSGS pulls out proposition names - last part of message.

   (defun propns-from-msgs (msgs)
   (mapcar #'last-element msgs)
   )

   (defun last-element (lst) (car (last lst)))

    

   ; *************************
   ; MAKE-CONSTRAINT-NET sets up a network of hypotheses
   ; based on two structures that may be analogous.
   ; Each structure can have any number of fields.

   (defun make-constraint-net (struc1 struc2)
   (let (links-num)
   (unless *silent-run?* (my-print '"Constructing constraint network for "
   struc1 '" and " struc2
   )
   )
   (setq *struc1* struc1)
   ; set up units and excitatory links for each field:

   (make-units-for-fields struc1 struc2)

   (unless *silent-run?*
   (and (my-print " Total number of units made: " (length *all-units*))
   (my-print " Total number of symmetric excitatory links: "
   (/ *total-links* 2)
   )
   )
   )
   (setq links-num *total-links*)
   ; set up links for queries:
   (if *look-for-queries?* (mapcar #'make-query-links *query-connections*))

   ; set up inhibitory links among concept hypotheses:
   (unless *silent-run?* (my-print " Making inhibitory links ..."))
   (inhibit-multiple-mappings (conc-from-struc struc1) *inhib-weight*)
   ; for 1-1 mapping:

   (if *map-one-one?* (inhibit-multiple-mappings (conc-from-struc struc2)
   (* *stop-many-one* *inhib-weight*)
   )
   )
   ; inhibitory links among object hypotheses (can be less than concepts)
   (inhibit-multiple-mappings (obj-from-struc struc1)
   (* *inhib-weight* *obj-conc-fraction*)
   )
   ; for 1-1 mapping:
   (if *map-one-one?* (inhibit-multiple-mappings (obj-from-struc struc2)
   (* *inhib-weight* *obj-conc-fraction*)
   )
   )
   ; inhibitory links to enforce 1-1 mapping of propositions:
   (inhibit-multiple-mappings *all-propositions*
   (* *inhib-weight* *propn-uniqueness*)
   )
   (unless *silent-run?*
   (and (my-print " Symmetric inhibitory links made: "
   (/ (- *total-links* links-num) 2)
   )
   (my-print '" Total symmetric links made: "
   (/ *total-links* 2)
   )
   )
   )
   (length *all-units*)
   ); end let
   )

   ; *************************
   ; MAKE-UNITS-FOR-FIELDS constructs units excitatory links
   ; for proposition mappings in particular fields.
   ; It ignores the different fields, combining them all
   ; into one big field, if map-all? is t.

   (defun make-units-for-fields (struc1 struc2)
   (if *map-all?*
   (and (combine-fields struc1)
   (combine-fields struc2)
   )
   )
   (do ((fields (get struc1 'fields) (cdr fields))
   (unit-num (length *all-units*))
   (link-num *total-links*)
   )
   ((null fields) 'done)
   ; repeat:
   (setq unit-num (length *all-units*))
   (setq link-num *total-links*)
   (unless *silent-run?*
   (my-print " Making units and excitatory links for field "
   (car fields) " ..."
   )
   )
   (make-hyp-units struc1 struc2 (car fields))
   (unless *silent-run?*
   (and (my-print " Units made: "
   (- (length *all-units*) unit-num)
   )
   (my-print " Symmetric links made: "
   (/ (- *total-links* link-num) 2)
   )
   )
   )
   )
   )

   ; *************************
   ; COMBINE-FIELDS conglomerates different fields into one.

   (defun combine-fields (struc)
   (do ((fields (get struc 'fields) (cdr fields))
   (result nil)
   )
   ((null fields)
   (put struc 'all result)
   )
   (setq result (union result
   (get struc (car fields))
   )
   )
   )
   (put struc 'fields '(all))
   )

    

    

    

   ; *************************
   ; MAKE-HYP-UNITS sets up units representing hypotheses that
   ; predicates and their associated objects are analogous.
   ; New: also does propositions.

   (defun make-hyp-units (prob1 prob2 field)
   (do ((msgs (get prob1 field) (cdr msgs)))
   ; exit:
   ((null msgs) 'done)
   ; repeat:
   (make-hyp-units-for-msg (car msgs)
   (get prob2 field)
   )
   )
   )

   ; ****************************
   ; MAKE-HYP-UNITS-FOR-MSG makes units for a particular message.

   (defun make-hyp-units-for-msg (msg messages)
   ; set up null mappings:
   (if *use-nothing-maps?*
   (make-hyp-unit msg
   (list 'no-concept
   (list-of-n-elem (length (get-args msg))
   'no-object
   )
   'no-proposition
   )
   )
   )
   ; all the rest:
   (do ((msgs messages (cdr msgs)))
   ; exit:
   ((null msgs) 'done)
   ; repeat:
   (make-hyp-unit msg (car msgs))
   )
   )

   ; ****************************
   ; MAKE-HYP-UNIT creates units corresponding to the hypotheses
   ; that two messages are analogous, i.e. that their concepts, arguments,
   ; and whole propositions correspond.
   ; Returns a list of the concept and proposition mappings created.
   ; Only concepts with same # of arguments are candidates.
   ; Structure of units:
   ; Unit-name
   ; Activation: -1 ... 1
   ; Linked-from: list of units.

   ;;; The new version checks to see if new-propn-unit already exists.

   ; PT 5-22-98 Modify mapping in line with DRAMA, so that on
   ; the first pass only semantically similar concepts are mapped;
   ; then, on the second pass, unmapped propositions can be
   ; mapped without this restriction, to ensure that everything
   ; in the source has a chance to map.

   (defun make-hyp-unit (msg1 msg2)
   (let ((conc1 (car msg1))
   (conc2 (car msg2))
   (propn1 (last-element msg1))
   (propn2 (last-element msg2))
   (args1 (get-args msg1))
   (args2 (get-args msg2))
   (new-conc-unit nil)
   (new-propn-unit nil)
   (result nil)
   (sem-sim? nil)
   )
   ; if same # args:
   (cond ((and *ignore-preds*
   (member conc1 *ignore-preds*)
   (member conc2 *ignore-preds*))
   )
   ((and (= (length args1) (length args2)) ; cond 1
   ; don't match objects and propositions:
   (type-compatible args1 args2 *all-propositions*)
   )
   (my-print "Mapping: " msg1)
   (my-print " and " msg2)
   (setq new-conc-unit (catname conc1 conc2))
   ; if concept unit not already made, create it:
   (cond ( (not-member new-conc-unit *all-units*) ; cond 2
   ; set up unit
   (put new-conc-unit 'concerns (list conc1 conc2))
   ; note creation of unit:
   (note-unit new-conc-unit)
   (setq result (cons new-conc-unit result))
   ; calculate semantic similarity of concepts
   (setq sem-sim?
   (if *use-arcs-semantics?*
   (sem-sim-arcs conc1 conc2)
   (sem-similarity conc1 conc2)
   )
   )
   (if (/= sem-sim? 0)
   (make-sym-link 'special new-conc-unit
   sem-sim?
   )
   )
   ; record hypotheses about a concept:
   (record-hypothesis conc1 new-conc-unit)
   (record-hypothesis conc2 new-conc-unit)
   )
   ); end cond 2
   ; create proposition unit:
   (setq new-propn-unit (catname propn1 propn2))
   ;;; The cond around the following four lines is the fix that Eric Melz
   ;;; devised -- installed by Greg Nelson 12/07/89
   (cond ((not-member new-propn-unit *all-units*)
   (put new-propn-unit 'concerns (list propn1 propn2))
   (note-unit new-propn-unit)
   (record-hypothesis propn1 new-propn-unit)
   (record-hypothesis propn2 new-propn-unit)
   (setq result (cons new-propn-unit result))
   )
   )
   ; link proposition unit with concept unit:
   (make-sym-link new-propn-unit new-conc-unit *excit-weight*)

   ; if msg2 from target contains a query, treat specially:
   (cond ( (and *look-for-queries?* ; cond 3
   (contains-query (get-args msg2))
   )
   (setq *query-connections*
   (cons (list new-propn-unit
   new-conc-unit
   (get-args msg1)
   (get-args msg2)
   )
   *query-connections*
   )
   )
   )
   ; else make excitatory links with objects:
   (t (setq *object-units* (make-obj-units (get-args msg1)
   (get-args msg2)
   *init-activ*
   )
   )
   (make-excit-links new-propn-unit
   *object-units*
   *excit-weight*
   )
   (if *link-concepts-objects?*
   (make-excit-links new-conc-unit
   *object-units*
   *excit-weight*
   )
   )
   )
   ) ; end cond 3
   )
   ) ; end cond 1
   ) ; end let
   )

   ; **********************************
   ; TYPE-COMPATIBLE ensures for all members of two sequences (A B C) and (1
   2 3)
   ; that if A is a member of a set, then so is 1.

   (defun type-compatible (lst1 lst2 set)
   (do ((seq1 lst1 (cdr seq1))
   (seq2 lst2 (cdr seq2))
   )
   ; if everything ok, return t.
   ((null seq1) t)
   ; check all pairs:
   (if (or (and (member (car seq1) set)
   (not-member (car seq2) set)
   )
   (and (not-member (car seq1) set)
   (member (car seq2) set)
   )
   )
   (return nil)
   )
   )
   )

    

   ; **********************************
   ; RECORD-HYPOTHESIS notes that a unit provides one of the possible
   ; mappings for concept, object, or proposition.

   (defun record-hypothesis (thing unit)
   (put thing 'constraint-hyps
   (cons-if-new unit (get thing 'constraint-hyps))
   )
   )
   ; *******************************
   ; CONTAINS-QUERY checks whether a list contains a query to be
   ; filled in, as indicated by an argument that starts with a "?".

   (defun contains-query (list-of-atm)
   (do ((lst list-of-atm (cdr lst))
   (result nil)
   )
   ; exit:
   ((null lst) result)
   ; repeat:
   (if (atom-begins (car lst) #\?)
   (setq result (cons (car lst) result))
   )
   )
   )

   ; *******************************
   ; MAKE-QUERY-LINKS sets up links based on a queries.
   ; The basic idea here is that units that provide a chance of
   ; answering a query will be favored over those that don't.

   (defun make-query-links (lst-of-four)
   (let ((propn-unit (car lst-of-four))
   (conc-unit (second lst-of-four))
   (arguments1 (third lst-of-four))
   (arguments2 (fourth lst-of-four))
   )

   (unless *silent-run?*
   (my-print '"Making query links: " propn-unit " " conc-unit '" "
   arguments1 '" " arguments2
   )
   )
   (do ((args1 arguments1 (cdr args1))
   (args2 arguments2 (cdr args2))
   (result-args1 arguments1)
   (result-args2 arguments2)
   (new-obj-units nil)
   (query-args nil)
   )
   ; exit:
   ((null args1)
   ; make normal object units:
   (setq new-obj-units
   (make-obj-units result-args1
   result-args2
   *init-activ*
   )
   )
   (cond ((and *look-for-queries?* (not *silent-run?*))
   (my-print '"DEBUGGING make-query-links ")
   (my-print '"new-obj-units " new-obj-units)
   (my-print '"result args " result-args1 result-args2)
   )
   )
   ; make normal links:

   (make-sym-link propn-unit
   conc-unit
   *excit-weight*
   )
   (make-excit-links propn-unit
   new-obj-units
   *excit-weight*
   )
   ; make special query links:
   (make-query-links-for-objs query-args
   (cons conc-unit new-obj-units)
   )
   )
   ; repeat:
   (cond ( (atom-begins (car args2) #\?)
   (setq query-args (cons (car args1) query-args))
   (setq result-args1 (remove (car args1) result-args1))
   (setq result-args2 (remove (car args2) result-args2))

   )
   )
   ) ; end do
   ) ; end let
   )
   ; *******************************
   ; MAKE-QUERY-LINKS-FOR-OBJS makes links among each member of a set of
   ; constraint hypotheses (e.g. all hypotheses concerning obj1)
   ; and the pragmatic unit.

   (defun make-query-links-for-objs (list-of-objects units)
   (create-pragmatic-unit)
   (do ((objs list-of-objects (cdr list-of-objects)))
   ; exit:
   ((null objs) 'done)
   ; repeat:
   (make-query-links-for-obj (car objs) units)
   )
   )

   ; *******************************
   ; MAKE-QUERY-LINKS-FOR-OBJ does it for one object.

   (defun make-query-links-for-obj (obj units)
   (do ((unts units (cdr unts)))
   ; exit
   ((null unts) 'done)
   ; repeat:
   (make-excit-links 'pragmatic
   (get obj 'constraint-hyps)
   *import-weight*
   )
   (unless *silent-run?*
   (my-print "Units important for querying: "
   (get obj 'constraint-hyps)
   )
   )
   )
   )

   ; *****************************
   ; NUMBER-DUPS looks for pairs of duplicate arguments: e.g.
   ; (a b a) and (d e d). Not quite perfect.

   (defun number-dups (list1 list2)
   (do ((lst (make-pairs list1 list2) (cdr lst))
   (result 1)
   )
   ; exit:
   ((null lst) result)
   ; repeat:
   (if (memberlist (car lst) (cdr lst))
   (setq result (+ result 1))
   )
   )
   )

   ; *******************************
   ; MAKE-PAIRS takes two lists, and returns a list of pairs,
   ; with the nth atom of list1 paired with the nth of list2.
   ; [from pi/analog.l].

   (defun make-pairs (lst1 lst2)
   (cond ( (not-equal (length lst1) (length lst2)) nil)
   ( (null lst1) nil )
   ( t (cons (list (car lst1) (car lst2))
   (make-pairs (cdr lst1) (cdr lst2))
   )
   )
   )
   )

   ; ******************************
   ; SEM-SIMILARITY calculates the semantic similarity between
   ; two concepts. It should use an overlapping feature count,
   ; but see particular data files for *no-concept-weight*s.
   ; PT, 5-95. For ACME in COHERE, don't do full ARCS similarity
   ; judgment, but just look for identical or similar predicates.

   (defun sem-similarity (conc1 conc2)
   (cond ( (equal conc1 conc2) *ident-weight*) ; concepts are the same
   ( (equal conc2 'no-concept) *no-concept-weight*) ; null mapping
   ( (similar? conc1 conc2) (similar? conc1 conc2)) ; concepts similar
   (t *no-sim-weight*)

   )
   )

   ; SIMILAR ; fixed 11-98 PT

   (defvar *similarities* nil)

   (defun similar (conc1 conc2 val)
   (setq *similarities* (cons (list (list conc1 conc2) val) *similarities*))
   )

    

   ; SIMILAR? checks the list *similarities* for a measure of
   ; the similarity of two concepts.

   (defun similar? (conc1 conc2)
   (do ((sims *similarities* (cdr sims)))
   ((null sims) nil)
   ; repeat
   (if (or (equal (list conc1 conc2) (caar sims))
   (equal (list conc2 conc1) (caar sims))
   )
   (return (second (car sims)))
   )
   )
   )

   ; NOSIMILAR

   (defun nosimilar ()
   (setq *no-concept-weight* nil)
   )

    

    

    

   ; Here's how it should be if Wordnet were comprehensive enough:
   ; SEM-SIMILARITY-FUTURE calculates the semantic similarity between
   ; two concepts. Typically, this should be:
   ; identical: .1
   ; synonymous: .08
   ; coordinates: .06 (i.e. are both kinds of same superordinate,
   ; or parts of same whole)

   (defun sem-similarity-future (conc1 conc2)
   (cond ( (eq conc1 conc2) *ident-weight*) ; concepts are the same
   ( (eq conc2 'no-concept) *no-sim-weight*) ; null mapping
   ( (synonymous conc1 conc2) *synon-weight*)
   ( (coord conc1 conc2) *coord-weight*)
   (t *no-sim-weight*)
   )
   )
   ; *************************
   ; SYNONYMOUS

   (defun synonymous (conc1 conc2)
   (or (memberlist (list conc1 conc2) *synonyms*)
   (memberlist (list conc2 conc1) *synonyms*)
   )
   )

   ; COORD identifies concepts that are parts of the same whole or
   ; subkinds of the same kind.

   (defun coord (conc1 conc2)
   (or (memberlist (list conc1 conc2) *same-kinds*)
   (memberlist (list conc2 conc1) *same-kinds*)
   (memberlist (list conc1 conc2) *same-parts*)
   (memberlist (list conc2 conc1) *same-parts*)
   )
   )

   ; *************************
   ; MAKE-OBJ-UNITS-FROM-OBJECTS sets up hypotheses about
   ; about object maps.

   (defun make-obj-units-from-objects (struc1-objs struc2-objs)
   (do ((s1-objs struc1-objs (cdr s1-objs))
   (units-made nil)
   )
   ; exit: return units made:
   ((null s1-objs) units-made)
   ; while repeating:
   (setq units-made
   (union units-made
   (make-obj-units-from-1-obj (car s1-objs)
   struc2-objs
   )
   )
   )
   )
   )

   ; **************************
   ; MAKE-OBJ-UNITS-FROM-1-OBJ does it for 1.

   (defun make-obj-units-from-1-obj (obj objects)
   (do ((obs objects (cdr obs))
   (units-made (if *use-nothing-maps?*
   (list (make-obj-unit obj 'no-object *init-activ*)))
   ;else
   nil
   )
   )
   ; exit: return units
   ((null obs) (remove nil units-made))
   ; repeat:
   (setq units-made
   (cons (make-obj-unit obj (car obs) *init-activ*)
   units-made
   )
   )
   )
   )

   ; **************************
   ; EXCIT-QUERIES sets up special excitatory links for
   ; query objects starting with "?".
   ; Excitatory links are set up between each query object mapping to
   ; an element and all units involving that element.

   (defun excit-queries (quer)
   (do ((quer-maps (get quer 'constraint-hyps) (cdr quer-maps)))
   ((null quer-maps) 'done) ; (important (list quer)) - retards settling.
   ; repeat:
   (my-print (car quer-maps) " query map")
   (make-excit-links (car quer-maps)
   (other-element-maps quer (car quer-maps))
   *excit-weight*
   )
   (make-inhib-links (get quer 'constraint-hyps) *inhib-weight*)
   )
   )

   ; *************************
   ; OTHER-ELEMENT-MAPS gives for an element E all
   ; the units concerning the mapping of all elements that
   ; map to E.

   (defun other-element-maps (el unit)
   (remove unit
   (get (other-from-pair el
   (get unit 'concerns)
   )
   'constraint-hyps
   )
   )
   )

   ; **************************
   ; To calculate the best match:
   ; **************************
   ; BEST-ANALOGY uses the hypothesis units to judge what is the
   ; best overall match:

   (defun best-analogy (prob)
   (unless *silent-run?*
   (my-print '"Calculating the best mappings after "
   *total-times* '" cycles."
   )
   )
   (setq *best-matches* nil)
   (mapcar #'best-match (union (conc-from-struc prob)
   (obj-from-struc prob)
   )
   )
   (if *stop-when-matched?*
   (cond ( (null (set-difference *desired-matches* *best-matches*
   :test #'equal
   )
   )
   (setq *stop-run?* t)
   (unless *silent-run?*
   (my-print '"Desired match accomplished at cycle "
   *total-times*
   )
   )
   )
   )
   )
   )

   (defun ba () (best-analogy *struc1*))

   ; **********************************
   ; FIND-ANALOG-MESSAGES constructs analogous messages.

   (defun find-analog-messages (lst-of-preds prob part-of-prob)
   (do ((preds lst-of-preds (cdr preds)))
   ; exit:
   ((null preds) nil)
   ; repeat:
   (find-analog-message (car preds) prob part-of-prob)
   )
   )

    

   ; **********************************
   ; FIND-ANALOG-MESSAGE constructs an analogous message for a predicate
   ; based on the best match of objects.

   (defun find-analog-message (pred problem part-of-problem)
   (do ((msgs (get problem part-of-problem) (cdr msgs))
   (result nil)
   )
   ; exit:
   ((null msgs) nil)
   ; repeat:
   (cond ( (> (sem-similarity pred (caar msgs)) *coord-weight*)
   (setq result (list (caar msgs)
   (mapcar #'best-match (get-args (car msgs)))
   )
   )
   (my-print '"Analogous message is " result)
   (return result)
   )
   )
   )
   )

   ; ********************************
   ; LIST-OF-N-ELEM makes a list consisting of n occurrences of an element.

   (defun list-of-n-elem (number element)
   (do ((num number (- num 1))
   (lst nil)
   )
   ((= 0 num) lst)
   (setq lst (cons element lst))
   )
   )

   ; ******************************
   ; PURPOSE: contains functions shared by ACME and ARCS
   ; **************************
   ; PRINT-GOOD reports on goodness of fit of analogy.

   (defun print-good ()
   (my-print '"Goodness of network: " (goodness *all-units*))
   )

   ; *****************************
   ; MAKE-OBJ-UNITS makes hypotheses relating pairs of
   ; ordered sequences of arguments.

   (defun make-obj-units (arguments1 arguments2 activation)
   (do ((args1 arguments1 (cdr args1))
   (args2 arguments2 (cdr args2))
   (units-made nil (cons (make-obj-unit (car args1)
   (car args2)
   activation
   )
   units-made
   )
   )
   ; adjust excitation weight for duplicate arguments: see data/num.l
   (weight (if *watch-for-dup-arguments?*
   (* *excit-weight* (number-dups arguments1 arguments2))
   *excit-weight*))
   )
   ((null args1)
   (setq units-made (remove-duplicates units-made))
   (setq *object-units* (union units-made *object-units*))
   (if *link-objects?* (make-all-excit-links units-made weight))
   units-made
   )
   )
   )

   ; *****************************
   ; MAKE-OBJ-UNIT makes a unit corresponding to the hypothesis that
   ; two objects are identical. Changed for MCL, PT, 5-95.

   (defun make-obj-unit (obj1 obj2 activation)
   (let (new-unit)
   (setq new-unit (catname obj1 obj2))
   (cond ((not-member new-unit *all-units*)

   (put new-unit 'concerns
   (list obj1 obj2)
   )
   (note-unit new-unit)

   ; unless unit already got activation from make-hyp-unit
   (unless (get new-unit 'activation)
   (and (put new-unit 'activation activation)
   (put new-unit 'original-activation activation)
   )
   )
   (put obj1 'constraint-hyps
   (cons-if-new new-unit (get obj1 'constraint-hyps))
   )
   (put obj2 'constraint-hyps
   (cons-if-new new-unit (get obj2 'constraint-hyps))
   )
   (setq *all-objects* (union *all-objects* (list obj1 obj2)))
   new-unit
   )
   (t new-unit)
   )
   )
   )

   ; *************************
   ; INHIBIT-MULTIPLE-MAPPINGS sets up inhibitory links among competing
   ; hypotheses about a particular object or concept.
   ; Will not clobber excitatory links: see make-sym-link.
   ; For pragmatics sake, it treats queries differently, forming
   ; excitatory rather than inhibitory links.

   ; The original, old version, which is erroneous, is commented out.
   ; Eric Melz's newer version follows.

   ;(defun inhibit-multiple-mappings (list-of-objs-or-concs weight)
   ; (do ((lst (remove-nil-dup list-of-objs-or-concs) (cdr lst)))
   ; ; exit:
   ; ((null lst) *total-links*)
   ; (if (atom-begins (car lst) #\?) ; query
   ; (excit-queries (car lst))
   ; ; otherwise:
   ; (make-inhib-links (no-queries (get (car lst) 'constraint-hyps))
   ; weight
   ; )
   ; )
   ; )
   ;)

   ;;; This fixes a bug which mixes up source and target components when
   ;;; considering inhibitory connections. For example, num5=num3 was
   ;;; inhibited by num6=num5.

   (defun inhibit-multiple-mappings (list-of-objs-or-concs weight)
   (do ((lst (remove-nil-dup list-of-objs-or-concs) (cdr lst)))
   ; exit:
   ((null lst) *total-links*)
   (if (atom-begins (car lst) #\?) ; query
   (excit-queries (car lst))
   ; otherwise:
   (progn
   (make-inhib-links
   (no-queries
   (get-firsts (car lst) (get (car lst) 'constraint-hyps)))
   weight)
   (make-inhib-links
   (no-queries
   (get-seconds (car lst) (get (car lst) 'constraint-hyps)))
   weight))
   )
   )
   )

   ; **************************
   ; PRAGMATICS:
   ; **************************

   ; *******************************
   ; CHECK-IMPORTANCE checks a structure to see if it is of the type problem,
   ; and, if so, makes appropriate connections to the "pragmatic" unit.
   ; The units connected include all hypotheses about predicates and
   propositions
   ; in the goal, and all hypotheses about propositions and predicates of
   ; proposition mentioned in the goal. If *propns-import?* is nil, the
   ; proposition connections will not be made.

   (defun check-importance (struc)
   (prog (goal goalprops)
   (if (or
   (not (equal (get struc 'data-type) 'problem))
   (not (member 'goal (get struc 'fields)))
   )
   (return nil)
   )
   (setq goal (get struc 'goal))
   ; If any predicates have been mapped, IMPORTANT them.
   (important
   (remove-if-not '(lambda (el) (member el mapped-concepts))
   (mapcar #'get-pred goal)
   )
   )
   ; If any propositions have been mapped, IMPORTANT them.
   (if *propns-import?*
   (important
   (remove-if-not '(lambda (el) (member el mapped-propositions))
   (mapcar #'get-propn-name goal)
   )
   )
   )
   ; If any arguments which are propositions have been mapped,
   (setq goalprops
   (remove-if-not '(lambda (el) (member el mapped-propositions))
   (apply #'append (mapcar #'get-args goal))
   )
   )
   ; IMPORTANT their predicates,
   (important
   (remove-if-not '(lambda (el) (member el mapped-concepts))
   (mapcar #'pred-from-propn goalprops)
   )
   )
   ; and IMPORTANT them.
   (if *propns-import?* (important goalprops))
   )
   )

   ; *****************************
   ; PRESUMED notes that certain mapping hypotheses or desired or presumed.

   (defun presumed (list-of-units)
   (create-pragmatic-unit)
   (do ((units list-of-units (cdr units)))
   ((null units) 'done)
   ; repeat:
   (unless *silent-run?* (my-print (car units) '" is a presumed mapping."))
   (make-sym-link 'pragmatic (car units) *prag-weight*)
   )
   )

   (defun desired (lst) (presumed lst))

   ; *****************************
   ; IMPORTANT indicates that the system has a special interest
   ; in an element (concept, object, proposition) although it
   ; does not desire any particular map.

   (defun important (list-of-els)
   (create-pragmatic-unit)
   (do ((els list-of-els (cdr els)))
   ((null els) 'done)
   ;
   (unless *silent-run?* (my-print (car els) " is an important element."))
   (make-excit-links 'pragmatic
   (get (car els) 'constraint-hyps)
   *import-weight*
   )
   )
   )

   ; *****************************
   ; CREATE-PRAGMATIC-UNIT makes a special pragmatic unit if necessary.

   (defun create-pragmatic-unit ()
   (cond ((null *pragmatic-unit-made*)
   (put 'pragmatic 'activation 1) ; clamp it
   (setq *all-units* (remove 'pragmatic *all-units*))
   (setq *pragmatic-unit-made* t)
   )
   )
   )

   ; **************************

   ; NO-QUERIES removes mappings about queries from a list of units.

   (defun no-queries (lst)
   (do ((ls lst (cdr ls))
   (result nil)
   )
   ((null ls) result)
   (unless (or (is-query (car (get (car ls) 'concerns)))
   (is-query (second (get (car ls) 'concerns)))
   )
   (push (car ls) result)
   )
   )
   )

    

   ; IS-QUERY

   (defun is-query (atm)
   (atom-begins atm #\?)
   )

   ; *****************************
   ; BEST-MATCH figures out what concept or object has been found to be
   ; the best match.

   (defun best-match (conc-or-obj)
   (let (bm best-unit other-good-units)
   (setq best-unit (highest-l (get conc-or-obj 'constraint-hyps)
   'activation
   )
   )
   (if (listp best-unit)
   (setq best-unit (car (setq other-good-units best-unit)))
   )
   (setq bm (other-from-pair conc-or-obj (get best-unit 'concerns)))
   (my-print "Best mapping of " conc-or-obj " is " bm ". "
   (get best-unit 'activation)
   )
   (if other-good-units
   (do ((units (cdr other-good-units) (cdr units)))
   ((null units) nil)
   (setq bm (other-from-pair conc-or-obj (get (car units) 'concerns)))
   (my-print " tied with " bm ".")
   )
   )

   (if *show-others?*
   (good-enough-s conc-or-obj
   (remove best-unit
   (get conc-or-obj 'constraint-hyps)
   )
   )
   )
   (setq *best-matches* (cons (list conc-or-obj bm) *best-matches*))
   )
   )

   ; *********************************
   ; OTHER-FROM-PAIR pick the other out of a pair that includes the given
   atom.

   (defun other-from-pair (atm lst)
   (if (eq atm (car lst)) (second lst)
   (car lst)
   )
   )

   ; **********************************
   ; GOOD-ENOUGH-S checks whether other mappings are over a threshold.

   (defun good-enough-s (el units)
   (do ((unts units (cdr unts)))
   ((null unts) 'done)
   (good-enough el (car unts))
   )
   )

   ; **********************************
   ; GOOD-ENOUGH says if a mapping has an activation over a threshold.

   (defun good-enough (el unit)
   (if (> (get unit 'activation) *min-good*)
   (my-print " Mapping with "
   (other-from-pair el (get unit 'concerns))
   " is also possible: "
   (get unit 'activation)
   )
   )
   )

   ; ***********************************
   ; CATNAME is used by ACME and ARCS to put together hypothesis unit names.
   ; Conversion to MCL, PT, 5-95

   (defun catname (unit1 unit2)
   (read-from-string (coerce (append (coerce (princ-to-string unit1) 'list)
   (coerce "=" 'list)
   (coerce (princ-to-string unit2) 'list)
   )
   'string
   )
   )
   )

   ; CONCAT is similar to catname, but more general, used in PI.

   (defun concat (&rest concat-things)
   (read-from-string (coerce (apply #'append
   (mapcar #'atom-to-list
   concat-things
   )
   )
   'string
   )
   )
   )

   ; ATOM-TO-LIST turns an atom into a list

   (defun atom-to-list (atm)
   (coerce (princ-to-string atm) 'list)
   )

   ; NOT-MEMBER ; for use with symbols produced by catname

   (defun not-member (el lst)
   (cond ( (memberlist el lst) nil )
   ( t t)
   )
   )
   ; MEMBERLIST
   ; Because member otherwise uses eq
   (defun memberlist (el lst2)
   (member el lst2 :test 'equal)
   )

    

    

    

   ;;; The original BEST-MATCH procedure has a bug where if the concept or
   object
   ;;; is in the target, it will also consider these mappings (i.e. from
   target
   ;;; to source rather than from source to target). This patch fixes that by
   ;;; adding an extra "filter" to the form (get conc-or-obj
   'constraint-hyps),
   ;;; namely, it only retrieves units in which conc-or-obj is the first
   element
   ;;; of the mapping unit.

    

   (defun get-firsts (conc-or-obj units)
   (let ((ret nil))
   (dolist (unit units)
   (when (equal (first (get unit 'concerns)) conc-or-obj)
   (setf ret (cons unit ret))))
   ret))

   ;;; get-seconds is like get-firsts except it does it for the target
   (defun get-seconds (conc-or-obj units)
   (let ((ret nil))
   (dolist (unit units)
   (when (equal (second (get unit 'concerns)) conc-or-obj)
   (setf ret (cons unit ret))))
   ret))
