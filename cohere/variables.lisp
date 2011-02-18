   ; FILE: variables.lisp
   ; PURPOSE: global variable initializations for networks
   ; PROGRAMMER: Paul Thagard, Greg Nelson
   ; CREATED: 12-9-87
   ; UPDATED: 5-13-88
   ; 10-20-97 PT

    

   ; does not need compiling.

    

   ; ******************************************************
   ; Global variables:
   ; ******************************************************

   ; FOR COHERE

   (defvar *problem* nil)
   (defvar *show-solutions* 25)
   (defvar *all-constraints* nil)
   (defvar *weight-of-all-constraints* 0)
   (defvar *connect-solution* nil)
   (defvar *count-solution* nil)
   (defvar *point-5-solution* nil)
   (defvar *greedy-solution* nil)
   (defvar *greedy-flips* 30 "number of flips in greedy algorithm")
   (defvar *number-units* nil "association list of numbers and unit names")
   (defvar *total-units* 0 "length of *all-units*")
   (defvar *eval-mode* 'tempered) ; coherence mode: pure, tempered, or
   foundational
   (defvar *num-solutions* 0) ; number of solutions to be considered
   (defvar *max-num-solutions* 5000) ; maximum number of solutions to try
   (defvar *special-register* 1) ; hold value for *special-activation*
   (defvar *resonance-impact* 1) ; resonance has effect if value > 1

    

    

   ; PARAMETERS FOR NETWORK OPERATION

   (defvar *init-activ* .01 "Initial activation of units.")
   (defvar *default-activ* .01 "Default activation of units.")
   (defvar *excit-weight* .04 "Weight of excitatory links.")
   (defvar *inhib-weight* -.06 "Weight of inhibitory links.")
   (defvar *simpl-impact* 1 "Impact of complexity of structure on
   coherence.")
   (defvar *goal-excit* .04 "Default weight of links from special unit to
   basic goals.")
   (defvar *weak-incohere-impact* .2 "Impact of incohering subgoals.")
   (defvar *analogy-impact* 1 "Impact of analogous goal structures.")
   (defvar *special-activation* 1 "Activation of special unit.")
   (defvar *min-activation* -.99 "Minimum possible activation for a unit.")
   (defvar *max-activation* .99 "Maximum possible activation for a unit.")
   (defvar *goal-min-activation* 0 "minimum possible activation for a goal.")
   (defvar *decay-amount* 0.1 "Amount that units' activations decay over
   time.")
   (defvar *output-threshold* 0 "Minimum activation for an influential
   unit.") ; PT 2-93
   (defvar *min-settle* 25 "Minimum time at which a network can settle.")
   (defvar *asymptote* .0001 "Amount of change at which a unit has
   asymptoted.")
   (defvar *current-excit* 0.0 "Variable for Grossberg rule.")
   (defvar *current-inhib* 0.0 "Variable for Grossberg rule.")
   (defvar *where-to-print* *standard-output* "Option to change output
   stream.")
   (defvar *when-to-print* '(1 30 40 50 70 85 100 150) "When to print state
   of network.") ; PT 2-93
   (defvar *silent-run?* nil "Printout results.")
   (defvar *stop-settled?* t "Stop if the network has settled.")
   (defvar *settled?* nil "Network has settled.")
   (defvar *stop-run?* nil "Use grossberg's rule for network.")
   (defvar *tversky?* nil "Use altered form when activation is negative.")
   (defvar *grossberg?* nil "Alternative activation function.")
   (defvar *testnum* 1)
   (defvar *verbose* t)
   (defvar *max-times* 300 "maximum number of cycles of updating")
   (defvar *acme-mode* nil )

   ; FOR GRAPHICS ;

   (defvar *use-actgraph* t "Use the activation grapher.")
   (defvar *actgraph-window* nil "Name of activation grapher window.")
   (defvar *netgraph-window* nil "Name of network grapher window.")
   (defvar *grapher-font* nil "Name of the grapher font.")

   (defvar *debug* nil "Turn on the debugger.")
   (defvar *devowel-length* 30 "Length beyond which vowels will be taken
   out.")
   (defvar *time-step* 0 "Added by Paul.")

    

   (defvar *netwin-height* 6 "Height in inches of grapher window.")
   (defvar *netwin-width* 6 "Width in inches of grapher window.")

   (defvar *netwin-x* (* *netwin-width* *pixels-per-inch-x*) "Window width.")
   (defvar *netwin-y* (* *netwin-height* *pixels-per-inch-y*) "Window
   height.")
   (defvar *netwin-xs* nil "Window starting x position.")
   (defvar *netwin-ys* nil "Window starting y position.")
   (defvar *unit-location-alist* nil "A-list for relating locations with
   units, for mouse-selection.")
   (defvar *unit-radius* 5 "Default size for unit circles.")
   (defvar *nethoriz-spc* nil "X distance between adjacent units.") ;
   different than spc's for act-grapher
   (defvar *netvert-spc* nil "Y distance between adjacent units.")
   (defvar *drawing-mode* 'circle "Determines whether to draw circle or
   squares.")
   (defvar *x-radius* nil "X radius of the circle on which units are
   plotted.")
   (defvar *y-radius* nil "Y radius of the circle on which units are
   plotted.")
   (defvar *second-label* 'actstring "Put activation as a second label at
   each node.")
   (defvar *link-labels* 'weight-link-label
   "The function to print as label on links, when non-nil.")
   (defvar *link-width* 'weight-link-width
   "The function which determines widths, when non-nil.")
   (defvar *known-functions* nil
   "Variable listing which functions can be graphed.")

   (defvar *run-type* nil "If value is 'slow, events will be handled during
   run.")
   (defvar *last-init* nil "Name of grapher last initialized.")
   (defvar *trace-list* nil "List of units to trace.")

   (defvar *use-actgraph*) ; nil "Activation grapher is on or off.") ; these
   replace
   (defvar *use-netgraph*) ; nil "Network grapher is on or off.") ;
   *last-init*

    

   ; FOR ACME:

   (defvar *all-structures* nil)
   (defvar *struc1* nil)
   (defvar *start-units* nil)
   (defvar *goal-units* nil)
   (defvar *effector-units* nil)
   (defvar *object-units* nil)
   (defvar *sim-list* nil)
   (defvar *no-sim-weight* 0)
   (defvar *obj-conc-fraction* 1 ) ; inhibition of object hypotheses
   (defvar *stop-many-one* 1) ; if > 1, discourages many-one mappings
   (defvar *propn-uniqueness* 1) ; enforces 1-1 mappings of propositions
   (defvar *map-all?* nil) ; map regardless of fields

   (defvar *ident-weight* .1) ; similarity of concept to self
   (defvar *synon-weight* .08) ; similarity of synonyms
   (defvar *coord-weight* .06) ; similarity of coordinates
   (defvar *synonyms* nil)
   (defvar *same-kinds* nil)
   (defvar *same-parts* nil)

   (defvar *map-one-one?* t) ; do 1-1 mapping
   (defvar *no-concept-weight* 0) ; weight from special to null units
   (defvar *stop-when-matched?* nil) ; for automatic stopping on correct
   answer
   (defvar *best-matches* nil)
   (defvar *desired-matches* nil)
   (defvar *use-nothing-maps?* nil "If t, construct no-concept maps.")

    

   (defvar *look-for-queries?* nil) ; for query arguments
   (defvar *query-connections* nil)
   (defvar *query-weight-proportion* 1)
   (defvar *link-concepts-objects?* t) ; link concept hyps to object hyps
   directly
   (defvar *link-objects?* t) ; make links between object mappings
   (defvar *watch-for-dup-arguments?* nil) ; see dup-arguments
   (defvar *pragmatic-unit-made* nil) ; for pragmatic constraint

   (defvar *show-others?* t) ; show other good maps
   (defvar *min-good* .2) ; good enough to notice
   (defvar *prag-weight* .3) ; weight to pragmatic unit for presumed
   (defvar *import-weight* .1) ; importance links to pragmatic unit

   (defvar *use-arcs-semantics?* nil) ; to use arcs semantics file to make
   ; similarity judgements
   (defvar *use-auto-prag?* nil) ; to have function check-importance invoked
   (defvar *use-selection-list?* nil) ; for ARCS
   (defvar *selection-list* nil) ; for ARCS
   (defvar *all-preds* nil)
   (defvar *symmetric-concepts* nil)
   (defvar *ignore-preds* nil)
   (defvar *ALL-CONCEPTS* nil)
   (defvar *FEATURE-SELECTION* nil)
   (defvar *IGNORE-PREDS* nil)
   (defvar *ALL-OBJECTS* nil)
   (defvar *PROPNS-IMPORT?* 0)
   (defvar *SHOW-OTHERS?* nil)

    

   ; FOR ECHO:

   (defvar *echo2-mode* T "T if running ECHO2.")
   (defvar *experiment* nil)
   (defvar *all-explainers* nil)
   (defvar *all-explained* nil)
   (defvar *explan-data* nil)
   (defvar *data-excit* .05) ; excitation of data
   (defvar *special-activation* 1)
   (defvar *all-propositions* nil)
   (defvar *all-data* nil)
   (defvar *data-self-links?* nil)
   (defvar *analogy-impact* 1) ; impact of analogy
   (defvar *simpl-impact* 1) ; impact of simplicity
   (defvar *co-hyp-importance* 1) ; if < 1, links between cohypotheses are
   less.
   (defvar *self-link-data* nil)
   (defvar *data-init-activ* .01)
   (defvar *init-activ* .01)
   (defvar *decay-register* .05)
   (defvar *check-unexplained* nil) ; look for unexplained data
   (defvar *entail-impact* 1) ; if > 1, entailment excites more than expln.
   (defvar *contradictions* nil) ; pairs of contradictory propns
   (defvar *ad-hoc-factor* 1 "decrease excitatory links for ad hoc
   explanations")

    

   ; FOR DECO

   (defvar *all-propositions* nil "List of all propns.")
   (defvar *all-units* nil "List of all units.")
   (defvar *all-supergoals* nil "List of all supergoals.")
   (defvar *all-subgoals* nil "List of all subgoals.")
   (defvar *all-basic-goals* nil "List of all basic goals.")
   (defvar *contradictions* nil "List of lists of contradictory goals.")
   (defvar *asymptoted-units* nil "Units that have reached asymptote.")
   (defvar *total-links* 0 "Total number of links created.")
   (defvar *total-times* 0 "Number of settle cycles that have been run.")
   (defvar *experiment* nil "A description of the current experiment.")

   ; FOR HOTCO

   (defvar *emote* nil "Do updating for HOTCO.")
   (defvar *all-valence-units* nil "List of units that have valences.")
   (defvar *valence-weight* .05 "Default value for valence weight.")
   (defvar *truth-valence* 0 "Value of truth.")
   (setf (get 'special 'valence) 0) ; extent to which truth is desired???
   ; not quite the right interpretation, because true theories are
   ; wanted, but not ugly truths about someone being evaluated.
   (defvar *evaluation-units* nil) ; for HOTCO 2

    

    

   (proclaim '(type (float)
   min-activation max-activation output-threshold
   excit-weight inhib-weight decay-amount
   current-excit current-inhib
   )
   )
