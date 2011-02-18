   ;---------------------------------------------------------------------
   ; FILE : COHERE-START.lisp
   ; PURPOSE : Load all coherence files
   ; PROGRAMMER: Paul Thagard
   ; CREATED : 5-1-1995
   ; NOTES: Version 3.0 with HOTCO, 10-97
   ; Ported to G3 PowerPC, 4-98.
   ; HOTCO 2, 6-2000.
   ; WEB version available 3-2001.
   ;
   ;Copyright(c) Paul Thagard
   ; University of Waterloo. 1995, 1997, 2000, 2001.

   ;---------------------------------------------------------------------
   ; Load all the files necessary to operate COHERE:
   ;---------------------------------------------------------------------

   ; *********************************************************************

    

   (print "Welcome to COHERE, incorporating ACME, ECHO, DECO, IMP, HOTCO
   etc.")
   (print "Version 4.0. June, 2000")
   (print "This program is copyright (c) Paul Thagard 1996, 1997, 2000.")
   (print "Permission is granted for use for research purposes only.")

   ;Lisp code for initializing global variables.
   (print "Loading global variables.")
   (load "Sanders HD:LISP:COHERE:Lisp code:variables.lisp")
   (print "Global variables loaded.")

   ; Lisp code for utility functions.
   (print "Loading utility functions.")
   (load "Sanders HD:LISP:COHERE:Lisp code:utilities.lisp")
   (print "Utility functions loaded.")

   ; Lisp code for creating and running networks.
   (print "Loading constraint network functions.")
   (load "Sanders HD:LISP:COHERE:Lisp code:network.lisp")
   (print "Constraint network functions loaded.")

   ; Lisp code for ECHO
   (print "Loading ECHO.")
   (load "Sanders HD:LISP:COHERE:Lisp code:echo.lisp")
   (print "ECHO functions loaded.")

   ; Lisp code for DECO
   (print "Loading DECO.")
   (load "Sanders HD:LISP:COHERE:Lisp code:deco.lisp")
   (print "DECO functions loaded.")

   ; Lisp code for IMP
   (print "Loading IMP.")

   (load "Sanders HD:LISP:COHERE:Lisp code:imp.lisp")
   (print "IMP functions loaded.")

   ; Lisp code for ACME
   (print "Loading ACME.")
   (load "Sanders HD:LISP:COHERE:Lisp code:acme.lisp")
   (print "ACME functions loaded.")

   ; Lisp code for non-connectionist algorithms.
   (print "Loading non-connectionist coherence algorithms.")
   (load "Sanders HD:LISP:COHERE:Lisp code:cohere.pfsl")
   (load "Sanders HD:LISP:COHERE:Lisp code:greedy.pfsl")

   ; Lisp code for emotional coherence.
   (print "Loading HOTCO emotional coherence algorithms.")
   (load "Sanders HD:LISP:COHERE:Lisp code:hotco.lisp")

   ; Lisp code for graphics.
   (print "Loading graphics.")
   (load "Sanders HD:LISP:COHERE:Lisp code:graphics.pfsl")

   (print "All COHERE files loaded.")
   (print "Setting up graphics.")
   (start-graph 'act)
   (start-graph 'network)
   (defun rsa () (reset-act *all-units*))
   (defun da (unit) (draw-around unit))
   (print "COHERE ready.")

   ;---------------------------------------------------------------------
   ; Function: Default
   ; Purpose :Set all the default values to be used.
   ;---------------------------------------------------------------------
   (defun defaults ()
   (setq *asymptote* .0001)
   (decay .05)
   (excit .04)
   (inhib -.06)
   (output -0.99)
   )

    

    

   ; Set all the initial default values.
   (defaults)
