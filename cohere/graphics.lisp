   ; FILE: graphics.l
   ; PURPOSE: Lucid graphics for displaying networks (sg), line graphs (lg)
   ; and real-time activation graphs (ag).
   ; PROGRAMMER: Greg Nelson
   ; CREATED: 7-20-87
   ; UPDATED: 5-26-88
   ; UPDATED: 7-22-88 - made DEVOWEL a bit cleaner
   ; UPDATED: 9-6-88 - additions to function grapher (file labels, ELIMINATE)
   ; UPDATED: 9-7-88 - corrections to fg file reading functions
   ; UPDATED: 10-12-88 - fixed the spidergrapher for the new link format

   ; NEW PURPOSE: Macintosh Allegro graphics for networks and real-time
   activation
   ; for MacECHO
   ; PROGRAMMER (editor): David Gochfeld
   ; UPDATED: 6-4-90 - began to replace window initialization and drawing
   with
   ; Allegro code.
   ; Replaced *grapher-viewport* with *grapher-window* and
   ; *grapher-bitmap* with *grapher-grafport* (a "view")
   ; Macros and fuctions written for clear-bitmap, make-position,
   ; draw-line and stringblt.
   ; to 6-6-90
   ; 6-8-90 - Changed "grapher-window" to "actgraph-window" for
   ; use in End_ECHO_session in mac_interface.l.
   ; Began to excise code for circle activation graphs and
   ; for *acme-mode* from Activation Grapher section.
   ; Put in more constant for the Activation grapher, to replace
   ; Greg's magic numbers. See defvar section for activation-
   ; grapher, below.
   ; Set size of the view (*grapher-grafport*) to the same as the
   ; window (*actgraph-window*)
   ; 6-11-90 - Drawing FIXED: now draws activation graphs correctly into
   ; *grapher-grafport*.
   ; Use saving-window object class (from their example, modified
   ; slightly) to redraw grapher window after it's been covered.
   ; Don't need *grapher-grafport* -- draw directly into window.
   ; Now draws text onto bitmap as well as window, so the labels
   ; are also redrawn when the window is refreshed.
   ; 6-12-90 - Refresh macro to redraw window from contents of bitmap: used
   to
   ; make labels appear in the bitmap font as soon as labels and axes drawn.
   ; This is a hack, instead of trying to set the bitmap font, which is a
   ; real mess.
   ; Added creation of Network graph window, and put actgraph window in
   ; upper right corner for default.
   ; Deleted sections for line-grapher and function-grapher. (half the file).
   ; Changed draw-line to take a window argument -- draw in that window.
   ; Changed refresh-win to select the window before redrawing it, so it
   ; doesn't draw its contents over a window in front of it and emss things
   up.
   ; 6-13-90 - Network Grapher draws correctly:
   ; created function draw-circle which calls the Quickdraw oval commands.
   ; removed some fluff greg had that is no longer needed (particularly
   ; *second-label* and child-function stuff, and reduced childcnt
   calculations
   ; to one place -- draw-around.
   ; Changed name of kidsfunc to linked-units.
   ; Made calculations of x-radius and y-radius work in pixels.
   ; Replaced calls to font-fixed-width with calls to the toolbox routine
   ; _StringWidth. PROBLEM: may need to be done IN the correct window?
   ; 6-14-90 - Added mouse control to network grapher:
   ; *unit-loc-alist* contains the top, bottom, right, and left values at
   which
   ; each unit is located, associated with that unit.
   ; window-click-event-handler
   ; record-loc converts loc of unit to a rectangle containing unit, and
   stores
   ; the rectangle and unit in the alist.
   ; search-loc looks in alist for unit corresponding to point.
   ; loc-test is the test used by search-loc to search the alist.
   ; Links are now drawn in patterns depending on weight: positive is black,
   ; negative is light grey, and zero is grey.
   ; 6-15-90 - reset-act now sets *actwin-x*, *actwin-y*, and
   *graphs-per-line*
   ; depending on the current size of the window -- so if the window is
   ; resized before a run, graphs will be printed in it according to its
   ; new size. Resizing during a run doesn't affect the distribution
   ; of the graphs.
   ; 6-18-90 - reset-act now tells you to enter a new list of units to graph
   OR to
   ; resize the window if won't fit in the window. Also, show-act
   ; now checks to make sure the window is open before drawing.
   ; NOTE: when a window is shrunk, everything off the edge of the window
   will be lost. So
   ; users should be warned not to shrink the activation grapher window until
   done with
   ; the information printed in it.
   ; 4-29-98 - port to MCL 4.2 and incorporation with COHERE

    

    

   ;(in-package 'user)

   (require 'quickdraw "Sanders HD:Lisp:MCL 4.2:Library:QuickDraw.pfsl") ;
   loads quickdraw before loading this
   (require 'saving-window "Sanders HD:Lisp:COHERE:Lisp
   code:window_objects.l") ; creates "saving-window" object class

   ; General global variables
   (defvar *actgraph-window* nil "Name of activation grapher window.")
   (defvar *netgraph-window* nil "Name of network grapher window.")
   (defvar *grapher-font* nil "Name of the grapher font.")

   (defvar *debug* nil "Turn on the debugger.")
   (defvar *devowel-length* 30 "Length beyond which vowels will be taken
   out.")
   (defvar *time-step* 0 "Added by Paul.")
   (defvar default_activation .01 "This is defined elsewhere.")

    

   ; Global variables specific to network grapher
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

   ; Global variables specific to activation grapher
   (defvar *act-type* nil "Type of activation graph: 'circle or 'line") ;
   Don't use this anymore.
   (defvar *unit-list* nil "List of units currently being graphed.")
   (defvar *act-list* nil "Previous activations of current units.")
   (defvar *actwin-height* 4 "Height in inches of grapher window.")
   (defvar *actwin-width* 4 "Width in inches of grapher window.")

   (defvar *actwin-x* (* *actwin-width* *pixels-per-inch-x*) "Window width.")
   (defvar *actwin-y* (* *actwin-height* *pixels-per-inch-y*) "Window
   height.")
   (defvar *actwin-xs* nil "Window starting x position.")
   (defvar *actwin-ys* nil "Window starting y position.")

   (defvar *graph-hgt* 50 "Height of graph + label + space.")
   (defvar *graph-wid* 60 "Width of graph + space.")
   (defvar *graphs-per-line* (truncate *actwin-x* *graph-wid*) "Number of
   graphs per row in window.")
   (defvar *vert-spc* 5 "Vertical space between graphs.")
   (defvar *horiz-spc* 10 "Horizontal space between graphs.")
   (defvar *lbl-hgt* 9 "Height of label")
   (defvar *axis-hgt* (- *graph-hgt* (+ *vert-spc* *lbl-hgt*)) "Length of y
   axis in pixels.")
   (defvar *axis-wid* (- *graph-wid* *horiz-spc*) "Length of x axis in
   pixels.")
   (defvar *left-margin* 5 "Margin on left side of window.")
   ; Set *title-bar-height* to 10 to insure that everything is visible when ;
   Do I still need this?
   ; drawing labels above the axes. Otherwise set it to 0.
   (defvar *title-bar-height* 0 "Height of title bar at top of window.")
   (defvar *top-margin* (+ 0 *title-bar-height*) "Margin on top of window.")

   ; Should this be translated?
   ;(defun dumpwin (file)
   ; (shell (format nil "screendump -x ~a -y ~a -X ~a -Y ~a ~a"
   ; *winxs* *winys* *winx* *winy* file))
   ;)

    

   ; Grapher initialization function
   (defun start-graph (&optional (gmode nil) (restart nil)
   (hei *actwin-height*) ; Activation grapher is default
   (wid *actwin-width*)
   (xloc nil)
   (yloc nil))
   "Function to open a window for the grapher and set the global variables."
   (let ((pix-hei (* *pixels-per-inch-y* hei)) ; hei and wid are size in
   inches
   (pix-wid (* *pixels-per-inch-x* wid))) ; convert to pixel sizes.

   (cond ((equal gmode 'act) ; the following are specifically for opening the
   ; activation grapher window
   ; If no values given for xloc and yloc of window, place window in
   upper-right corner.
   (setq xloc (or xloc (- *screen-width* pix-wid)))
   (setq yloc (or yloc *menubar-bottom*))

   ; Open window if never opened, explicitly requested (restart), or if
   closed since last run
   (cond ((or
   (not (null restart))
   (null *actgraph-window*)
   (not (wptr *actgraph-window*))) ; t if window has been closed
   (setq *actgraph-window* (make-instance 'saving-window
   :window-title "Activation Grapher"
   :view-position (make-point xloc yloc)
   :view-size (make-point pix-hei pix-wid)))

   (setq *actwin-x* pix-wid) (setq *actwin-y* pix-hei)
   (setq *actwin-xs* xloc) (setq *actwin-ys* yloc)

   )
   )
   )

   ((equal gmode 'network) ; Open the network-grapher window
   ; If no values given for xloc and yloc of window, place window in
   lower-right corner.
   (setq xloc (or xloc (- *screen-width* pix-wid)))
   (setq yloc (or yloc (- *screen-height* pix-hei)))

   ; Open window if never opened, explicitly requested (restart), or if
   closed since last run
   (cond ((or
   (not (null restart))
   (null *netgraph-window*)
   (not (wptr *netgraph-window*))) ; t if window has been closed
   (setq *netgraph-window* (make-instance 'network-window
   :window-title "Network Grapher"
   :view-position (make-point xloc yloc)
   :view-size (make-point pix-hei pix-wid)))

   (setq *netwin-x* pix-wid) (setq *netwin-y* pix-hei)
   (setq *netwin-xs* xloc) (setq *netwin-ys* yloc)

   )
   )
   (setq *nethoriz-spc* (round (/ pix-wid 5)))
   (setq *netvert-spc* 45)
   )
   )
   ; (setq *last-init* gmode)
   )
   )

    

   ;;;;;;; General macros to translate Lucid sunview commands to Mac Allegro
   commands ;;;;;;;

   (defmacro make-position (x y) `(make-point ,x ,y))

   (defmacro draw-line (window pt1 pt2)
   `(progn (move-to ,window ,pt1)
   (line-to ,window ,pt2)))

   (defmacro refresh-win (win)
   `(progn (window-select ,win)
   (window-draw-contents ,win)))

   (defun clear-bitmap (win)
   (erase win) ; erase is defined in saving-window.l
   )

   (defun draw-circle (win cntr radius &optional (highlight nil))
   "Draw a circle centered at cntr with radius, filled if highlight is t,
   empty otherwise."
   (let ((left (- (point-h cntr) radius))
   (top (- (point-v cntr) radius))
   (right (+ (point-h cntr) radius))
   (bottom (+ (point-v cntr) radius)))
   (cond (highlight
   (paint-oval win (make-point left top) (make-point right bottom)))
   (t ; Just draw the darn thing, erasing whatever's under it.
   (erase-oval win (make-point left top) (make-point right bottom))
   (frame-oval win (make-point left top) (make-point right bottom)))
   ) ; note: all oval functions are defined in saving-window.l and require
   two points as args.
   )
   )

    

   ;;;;;;;;;;;;;;;;;;; Network grapher (sg)

    

    

   ; This is now in saving-windows.l, as an obfun for the new class
   network-window
   ;
   ;(defobfun (window-click-event-handler *netgraph-window*) (where)
   ; "Object function to respond to mouse clicks in *netgraph-window*, to
   allow selection of units."
   ; (usual-window-click-event-handler where)
   ; ; Probably what I'm going to need to do here is set up an a-list for
   each graph,
   ; ; and associate the locations of each unit in the graph with its
   coordinates.
   ; ; Then I'll need to look for the coordinates +/- *unit-radius* in each
   direction.
   ; ; Once I've got the unit name, I just clear the window and call
   (draw-around unit)
   ;
   ;; (print (point-string where))
   ;
   ; (let ((unit (search-loc where))) ; find unit in which mouse was clicked
   ; (if unit (draw-around unit)) ; if there is such a unit, draw it's
   connectivity
   ; )
   ;)

    

    

   (defun child-count (unit)
   "Function to return the number of connections to a given unit."
   (length (linked-units unit))
   )

   (defun x-rel (num childcnt &optional (xcent (round (/ *netwin-x* 2))))
   "Calculate x position of a given unit."
   (if (equal *drawing-mode* 'circle)
   (let (angle)
   (setq angle (* num (/ (* 2 pi) childcnt)))
   (round (+ xcent (* *x-radius* (cos angle))))
   )
   ; (let (percol delt col ncol) ;this is for the square format: ask Paul if
   I can trash this option
   ; ; otherwise I'll have to rewrite this code
   ; (setq delt (round (/ (bitmap-height *grapher-grafport*)
   ; (if (< childcnt 16) (+ 2 childcnt) (+ 5 childcnt)))))
   ; (setq ncol 1) (setq percol childcnt)
   ; (if (< delt *netvert-spc*) (prog () (setq delt (* delt 2))
   ; (setq percol (round (/ percol 2.0)))
   ; (setq ncol (* ncol 2))))
   ; (if (< delt *netvert-spc*) (prog () (setq delt (round (* delt 2)))
   ; (setq percol (round (/ percol 2.0)))
   ; (setq ncol (* ncol 2))))
   ; (setq col (- ncol (floor (/ num percol))))
   ; (cond ((equal col 1) (* 4 *nethoriz-spc*))
   ; ((equal col 2) *nethoriz-spc*)
   ; ((equal col 3) (* 2 *nethoriz-spc*))
   ; ((equal col 4) (* 3 *nethoriz-spc*)))
   ; )
   )
   )

   (defun y-rel (num childcnt &optional (ycent (round (/ *netwin-y* 2))))
   "Calculate y position of a given unit."
   (if (equal *drawing-mode* 'circle)
   (let (angle)
   (setq angle (* num (/ (* 2 pi) childcnt)))
   (round (+ ycent (* *y-radius* (sin angle))))
   )
   ; (let (cc temp delt ncol percol) ; square format: do I need to keep this?
   ; (setq cc childcnt)
   ; (setq delt (round (/ (bitmap-height *grapher-grafport*)
   ; (if (< cc 16) (+ 2 cc) (+ 5 cc)))))
   ; (setq ncol 1) (setq percol cc)
   ; (if (< delt *netvert-spc*) (prog () (setq delt (* delt 2))
   ; (setq percol (round (/ percol 2.0)))
   ; (setq ncol (* ncol 2))))
   ; (if (< delt *netvert-spc*) (prog () (setq delt (round (* delt 2)))
   ; (setq percol (round (/ percol 2.0)))
   ; (setq ncol (* ncol 2))))
   ; (setq temp (- ncol (floor (/ num percol))))
   ; (round (* (+ (mod num percol)
   ; (if (and (oddp temp) (equal ncol 4)) 0.5 1)
   ; ) delt))
   ; )
   )
   )

   (defun draw-around (unit)
   "Basic function to draw the connections to a given unit."
   (let (adj-lis xcent ycent childcnt
   (winsize (view-size *netgraph-window*)))
   (clear-bitmap *netgraph-window*)
   (setq *unit-location-alist* nil)
   (setq *netwin-x* (point-h winsize)) ; allows response to window resizing
   (setq *netwin-y* (point-v winsize))
   (setq *x-radius* (round (/ *netwin-x* 3.5)))
   (setq *y-radius* (round (/ *netwin-y* 2.5)))
   (setq xcent (round (/ *netwin-x* 2.0)))
   (setq ycent (round (/ *netwin-y* 2.0)))
   (setq adj-lis (linked-units unit))
   (setq childcnt (length adj-lis))
   (set-view-size *netgraph-window* winsize) ; allows response to window
   resizing
   (do ((tlist adj-lis (cdr tlist))
   (tunit nil nil)
   (count 0 (1+ count))
   (linkw nil nil)
   )
   ((null tlist) nil)
   (setq tunit (car tlist))
   (setq linkw (weight-link-width unit tunit)) ; get a pattern dependent on
   weight
   (set-pen-pattern *netgraph-window* linkw) ; set pen to the pattern
   (draw-line *netgraph-window*
   (make-position xcent ycent)
   (make-position (x-rel count childcnt xcent)
   (y-rel count childcnt ycent))
   )
   )
   (set-pen-pattern *netgraph-window* *black-pattern*) ; restore pen to black
   pattern
   (do ((tlist adj-lis (cdr tlist))
   (tunit nil nil)
   (count 0 (1+ count))
   (xpt (x-rel 0 childcnt xcent)
   (x-rel (1+ count) childcnt xcent)
   )
   (ypt (y-rel 0 childcnt ycent)
   (y-rel (1+ count) childcnt ycent)
   )
   (word nil nil)
   (str-pix-len nil nil)
   )
   ((null tlist) nil)
   (setq tunit (car tlist))
   (draw-unit tunit xpt ypt)
   (record-loc tunit xpt ypt)
   (setq word (weight-link-label unit tunit))
   (setq str-pix-len (with-pstrs ((str word))
   (#_StringWidth :ptr str :word))
   )
   (draw-string *netgraph-window* ;stringblt *grapher-grafport*
   (make-position
   (round (/ (- (+ xpt xcent) str-pix-len) 2.0))
   (+ (round (/ (+ ypt ycent) 2.0)) (round (/ *lbl-hgt* 2)))
   )
   ; *grapher-font* ; can't set font at this point
   word
   )
   )
   (draw-unit unit xcent ycent)
   ; if you wanted to refresh-win the window, you'd do it here.
   (refresh-win *netgraph-window*)
   )
   )

    

   (defun record-loc (unit x y)
   "Stores unit and rectangle containing it in the association-list."
   (let ((top (- y *unit-radius*))
   (bottom (+ y *unit-radius*))
   (left (- x *unit-radius*))
   (right (+ x *unit-radius*)))
   (setq *unit-location-alist* (acons (list (list top bottom) left right) ;
   key
   unit ; datum
   *unit-location-alist* ; a-list
   )
   )
   )
   )

   (defun search-loc (pt)
   "Looks in a-list for the unit corresponding to this point."
   (cdr (assoc pt *unit-location-alist* :test 'loc-test))
   )

   (defun loc-test (pt rect)
   "Test for searching a-list for unit corresponding to pt."
   (let ((x (point-h pt))
   (y (point-v pt))
   (vertrange (car rect))
   (horizrange (cdr rect)))
   (and (and (> y (car vertrange)) (< y (cadr vertrange)))
   (and (> x (car horizrange)) (< x (cadr horizrange))))
   )
   )

   (defun linked-units (unit)
   "Default function to determine what is linked to a given unit (for ACME)."
   (mapcar #'car (get unit 'links-from))
   )

   (defun weight-link-width (unit1 unit2)
   "Function which determines the width of the line in various cases."
   (let ((linkw (weight_of_link_between unit1 unit2)))
   (cond ((< 0 linkw) *black-pattern*)
   ((> 0 linkw) *light-gray-pattern*)
   ((equal 0 linkw) *gray-pattern*)
   ((equal 0.0 linkw) *gray-pattern*)
   )
   )
   )

   (defun weight-link-label (unit1 unit2)
   "Function which gives the label for the links (for ACME)."
   (princ-to-string (/ (round (* (weight_of_link_between unit1 unit2) 1000))
   1000.0
   )
   )
   )

   (defun actstring (unit)
   "Default function to determine the second label for a unit (ACME)."
   (princ-to-string (/ (round (* (act unit) 1000)) 1000.0))
   )

    

   (defun draw-unit (unit x y &optional (highlight nil))
   "Basic unit drawing function."
   (let (str-pix-len word)
   (debug-print (list "drawing circle for" unit))
   (draw-circle *netgraph-window* (make-position x y) *unit-radius*
   highlight)
   (setq word (string unit))
   (if (> (length word) *devowel-length*) (setq word (devowel word)))
   (setq str-pix-len (with-pstrs ((str word))
   (#_StringWidth :ptr str :word)))

   (draw-string *netgraph-window*
   (make-position
   (- x (round (/ str-pix-len 2.0)))
   (- y (+ *unit-radius* 5))) ; this 5 may be a function of *lbl-hgt* ?
   ;*grapher-font*
   word
   )
   (setq word (actstring unit))
   (setq str-pix-len (with-pstrs ((str word))
   (#_StringWidth :ptr str :word)))
   (draw-string *netgraph-window*
   (make-position
   (- x (round (/ str-pix-len 2.0)))
   (+ y (+ *unit-radius* 15))) ; this 15 may be a function of *lbl-hgt* ?
   ;*grapher-font*
   word
   )
   (debug-print (list "done drawing" unit))
   )
   )

    

    

    

   ;;;;;;;;;; Activation grapher (ag)

    

    

    

   (defun reset-act (&optional unit-list no-sort)
   "Prepare the grapher to trace the activations of units in unit-list."
   (let ((winsize (view-size *actgraph-window*)))
   (setq *actwin-x* (point-h winsize)) ; recalculate window dimensions and
   graph
   (setq *actwin-y* (point-v winsize)) ; distribution, in case window has
   been
   (setq *graphs-per-line* (truncate *actwin-x* *graph-wid*)) ; resized
   )
   (prog (x y word (glines (/ *actwin-y* *graph-hgt*)))
   (cond ((and (null unit-list) (null *unit-list*))
   (print "No previously defined unit-list exists.")
   (return nil))
   ((null unit-list) (setq unit-list *unit-list*))
   )
   (cond ((> (length unit-list) (* glines *graphs-per-line*)) ; more units to
   graph than will fit?
   (format t "~%Too many units to graph. Max is ~d units.~%"
   (round (* glines *graphs-per-line*)))
   (cond
   ((equal *run-type* 'slow)
   (let ((size (view-size *actgraph-window*)))
   (format t "Enter list of units to graph, or resize window and enter (): ")
   (cond ((and (null (setq unit-list (read))) ; If no list entered, and
   window not
   (equal size (view-size *actgraph-window*))) ; resized, terminate graphing.
   (format t "~%Not graphing any units.~%")
   (window-close *actgraph-window*) ; close window
   ; (setq *use-actgraph* nil) ; turn off grapher: this is a hassle
   (return nil) ; but otherwise show-act is called
   ) ; and you get an error
   ((and (> (point-h (view-size *actgraph-window*)) (point-h size))
   (not (< (point-v (view-size *actgraph-window*)) (point-v size))))
   (reset-act )
   (return-from reset-act nil)
   )
   )
   )
   )
   (t (format t "Terminating graphing.~%")
   (window-close *actgraph-window*)
   ; (setq *use_actgraph* nil) ; turn off grapher: hassle as above
   (return nil) ; Could have run_hyp_net check if
   ) ; window is open, but this is neater.
   )
   )
   )

   ;; these two lines have been commented out because sort won't behave
   ;; (basically it only returns the first item in the list)
   ;; (if (null no-sort)
   ;; (stable-sort unit-list #'string-lessp :key #'symbol-name))

   (setq *act-list* (rep default_activation (length unit-list)))
   (setq *unit-list* unit-list)
   (setq *time-step* 0)
   ; allows response to window resizing
   (set-view-size *actgraph-window* (view-size *actgraph-window*))
   (clear-bitmap *actgraph-window*)
   (do ((ul (cdr *unit-list*) (cdr ul))
   (un (car *unit-list*) (car ul))
   (pos 0 (1+ pos)))
   ((null un) nil)
   (setf (get un 'activation) default_activation)
   (setq x (+ (round (* (mod pos *graphs-per-line*)
   *graph-wid*)) *left-margin*))
   (setq y (+ (round (* (+ (truncate (/ pos *graphs-per-line*)) 1)
   *graph-hgt*))
   *top-margin*))
   (setq word (string un))
   (if (> (length word) *devowel-length*) (setq word (devowel word)))
   (draw-string *actgraph-window*
   (make-position x ;(+ y *graph-hgt*) ;this puts labels *vert-spc* pixels
   above axes
   y) ; while this puts labes 0 pixels below axes
   ;*grapher-font* ; maybe can implement this later
   word
   )
   (draw-line *actgraph-window* ;; DRAW AXES
   (make-position x (- y (+ *axis-hgt* *lbl-hgt*)))
   (make-position x (- y *lbl-hgt*))
   )
   (draw-line *actgraph-window*
   (make-position x (- y (+ (/ *axis-hgt* 2) *lbl-hgt*)))
   (make-position (+ x ;(round (/ (- *actwin-x* (* *horiz-spc*
   ; *graphs-per-line*))
   ; *graphs-per-line*))
   *axis-wid*
   )
   (- y (+ (/ *axis-hgt* 2) *lbl-hgt*)))
   )
   )
   (refresh-win *actgraph-window*) ; redraw everything in window so Labels
   are in
   ; same font as in bitmap. (Hack.)
   )
   )

    

   (defun show-act ()
   "The real function to show the activation of the current *unit-list*."
   (if (wptr *actgraph-window*)
   (let (nal)
   (do ((ul (cdr *unit-list*) (cdr ul))
   (al (cdr *act-list*) (cdr al))
   (un (car *unit-list*) (car ul))
   (ac (car *act-list*) (car al))
   (pos 0 (1+ pos)))
   ((null un) nil)
   (draw-delta-line pos
   (* (act un) (- 0 (/ *axis-hgt* 2)))
   (* ac (- 0 (/ *axis-hgt* 2))))
   (setq nal (append nal (list (act un))))
   )
   (setq *act-list* nal)
   (setq *time-step* (1+ *time-step*))
   )
   )
   )

   (defun draw-delta-line (pos new-a old-a)
   "Function to redraw the individual units with a new activation."
   (let (x y (xs (/ *axis-wid* (car (last *when-to-print*)))))
   (setq x (+ (round (* (mod pos *graphs-per-line*)
   *graph-wid*))
   (* *time-step* xs) *left-margin*))
   (setq y (+ (+ (* (truncate pos *graphs-per-line*) *graph-hgt*) (/
   *axis-hgt* 2))
   *top-margin* *vert-spc*
   )
   )
   (draw-line *actgraph-window*
   (make-position (1+ (round (- x xs))) (round (+ y old-a)))
   (make-position (round x) (round (+ y new-a)))
   )
   )
   )

    

   (defun act (unit)
   "Function to determine the activation of a unit."
   (get unit 'activation)
   )

   (defun debug-print (message)
   (if (null *debug*) nil (print message)))
