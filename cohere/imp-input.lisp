   ; Note: the first file has been modified to run in COHERE. The original
   IMP versions

   ; follow.

   ; File P1.disambiguation
   ; Purpose: Stereotype disambiguates ambiguous behavior
   ;
   ;
   ; Programmer: Paul Thagard
   ; Created : October , 1994
   ; Modified for COHERE, 5-95, PT.

    

   ; condition 1: black pushes someone
   (defun c1a ()
   (clear-net)
   (observed 'Jamal1 'black)
   (observed 'Jamal1 'pushed-someone)
   (associate 'black 'aggressive)
   (associate 'aggressive 'violent-push)
   (associate 'aggressive 'jovial-shove -1)
   (associate 'pushed-someone 'violent-push)
   (associate 'pushed-someone 'jovial-shove)
   (associate 'violent-push 'jovial-shove -1)
   (reset-act *all-units*)
   (eval-cohere )
   )

   ; condition 2: white pushes someone

   (defun c1b ()
   (clear-net)
   (observed 'Hugh1 'white)
   (observed 'Hugh1 'pushed-someone)
   ; (associate 'white 'aggressive -1)
   (associate 'aggressive 'violent-push)
   (associate 'aggressive 'jovial-shove -1)
   (associate 'pushed-someone 'violent-push)
   (associate 'pushed-someone 'jovial-shove)
   (associate 'violent-push 'jovial-shove -1)
   (eval-cohere)
   )

   (defun cp1 ()
   (1a) (1b)
   (my-print "P1 desired result: Jamal1 more violent than jovial.")
   (my-print " Hugh1 more jovial than violent")
   (compare 'jamal1 'hugh1 'violent-push)
   (compare 'jamal1 'hugh1 'jovial-shove)
   )

   (my-print "P1 loaded.")

   ; File P1.disambiguation
   ; Purpose: Stereotype disambiguates ambiguous behavior
   ;
   ;
   ; Programmer: Paul Thagard
   ; Created : October , 1994

    

    

   ; condition 1: black pushes someone
   (defun 1a ()
   (clear-net)
   (observed 'Jamal1 'black)
   (observed 'Jamal1 'pushed-someone)
   (associate 'black 'aggressive)
   (associate 'aggressive 'violent-push)
   (associate 'aggressive 'jovial-shove -1)
   (associate 'pushed-someone 'violent-push)
   (associate 'pushed-someone 'jovial-shove)
   (associate 'violent-push 'jovial-shove -1)
   (imp-run 'jamal1)
   )

   ; condition 2: white pushes someone

   (defun 1b ()
   (clear-net)
   (observed 'Hugh1 'white)
   (observed 'Hugh1 'pushed-someone)
   ; (associate 'white 'aggressive -1)
   (associate 'aggressive 'violent-push)
   (associate 'aggressive 'jovial-shove -1)
   (associate 'pushed-someone 'violent-push)
   (associate 'pushed-someone 'jovial-shove)
   (associate 'violent-push 'jovial-shove -1)
   (imp-run 'hugh1)
   )

   (defun p1 ()
   (1a) (1b)
   (my-print "P1 desired result: Jamal1 more violent than jovial.")
   (my-print " Hugh1 more jovial than violent")
   (compare 'jamal1 'hugh1 'violent-push)
   (compare 'jamal1 'hugh1 'jovial-shove)
   )

   (my-print "P1 loaded.")

   ; File P2.influence meaning
   ; Purpose: stereotype influences meaning of trait
   ;
   ;
   ; Programmer: Paul Thagard
   ; Created : October 24, 1994

    

    

   ; condition 1: Lawyer argues
   (defun 2a ()
   (clear-net)
   (observed 'Reg2 'lawyer)
   (associate 'lawyer 'aggressive)
   (associate 'lawyer 'up-mid-class)
   (associate 'lawyer 'verbal)
   (associate 'aggressive 'punch)
   (associate 'aggressive 'argue)
   (associate 'up-mid-class 'punch -1)
   (associate 'verbal 'argue)
   (imp-run 'Reg2)
   )

   ; condition 2: Construction worker punches
   (defun 2b ()
   (clear-net)
   (observed 'Hank2 'construction-worker)
   (associate 'construction-worker 'aggressive)
   (associate 'construction-worker 'work-class)
   (associate 'construction-worker 'unrefined)
   (associate 'aggressive 'punch)
   (associate 'aggressive 'argue)
   (associate 'work-class 'punch)
   (associate 'unrefined 'punch)
   (imp-run 'Hank2)
   )

   (defun p2 ()
   (2a) (2b)
   (my-print "P2 desired result: Reg2 more argues than punches.")
   (my-print " Hank2 more punches than argues")
   (compare 'Reg2 'Hank2 'argue)
   (compare 'Reg2 'Hank2 'punch)
   )

   (my-print "P2 loaded.")

   ; File P3.individuating subtype ALTERNATE
   ; Purpose Individuating information determines which subtype of a
   ; steretype is activated
   ;
   ;
   ; Programmer: Paul Thagard
   ; Created : October 26, 1994

    

   ; condition 1: Black is well-dressed
   (defun 3a ()
   (clear-net)
   (observed 'Sid3 'black)
   (observed 'Sid3 'well-dressed)
   (associate 'black 'ghetto-black )
   (associate 'black 'black-businessman)
   (associate 'well-dressed 'ghetto-black -1 )
   (associate 'well-dressed 'black-businessman )
   (associate 'ghetto-black 'black-businessman -1)
   (associate 'ghetto-black 'aggressive )
   (associate 'black-businessman 'aggressive -1)
   (imp-run 'Sid3)
   )

   ; condition 2: Black is not well-dressed
   (defun 3b ()
   (clear-net)
   (observed 'Bro3 'black)
   (associate 'black 'ghetto-black )
   (associate 'black 'black-businessman)
   (associate 'well-dressed 'ghetto-black -1)
   (associate 'well-dressed 'black-businessman )
   (associate 'ghetto-black 'black-businessman -1)
   (associate 'ghetto-black 'aggressive )
   (associate 'black-businessman 'aggressive -1)
   (imp-run 'Bro3)
   )

   (defun p3 ()
   (3a) (3b)
   (my-print "P3 desired result: Bro3 is more aggressive than Sid3.")
   (compare 'Bro3 'Sid3 'aggressive)
   )

   (print "")
   (my-print "P3 loaded.")

   ; File P4.no individuating
   ; Purpose Stereotypes in the absence of individuating information color
   ; impressions
   ;
   (print "")
   (my-print "Loading P4")

   ; condition 1: Construction worker expected to be aggressive.
   (defun 4a ()
   (clear-net)
   (observed 'Jim4 'construction-worker)
   (associate 'construction-worker 'aggressive)
   (imp-run 'Jim4)
   )

   ; condition 2: Housewife is not aggressive.
   (defun 4b ()
   (clear-net)
   (observed 'Joan4 'housewife)
   (associate 'housewife 'aggressive -1)
   (imp-run 'Joan4)
   )

   (defun p4 ()
   (4a) (4b)
   (my-print "Desired result: Jim is more aggressive than Joan.")
   (compare 'Jim4 'Joan4 'aggressive)
   )

   ; File P5.unambiguous diagnostic behavior
   ; Purpose Stereotype does not affect trait ratings in presence of
   ; unambiguous diagnostic behavior
   ;
   ;
   ; Programmer: Paul Thagard
   ; Created : October26, 1994

    

    

   ; condition 1: Construction worker expected to be aggressive.
   (defun 5a ()
   (clear-net)
   (observed 'Jim5 'construction-worker)
   ; (observed 'Jim5 'punch-adult)
   (associate 'construction-worker 'aggressive)
   (associate 'punch-adult 'aggressive 3)
   (associate 'construction-worker 'punch-adult 1) ;;;!!!
   (imp-run 'Jim5)
   )

   ; condition 2: Housewife is not aggressive.
   (defun 5b ()
   (clear-net)
   (observed 'Joan5 'housewife)
   ; (observed 'Joan5 'punch-adult)
   (associate 'housewife 'aggressive -1)
   (associate 'housewife 'punch-adult -1) ;;; !!!
   (associate 'punch-adult 'aggressive 3)
   (imp-run 'Joan5)
   )

   (defun p5 ()
   (5a) (5b)
   (my-print "Desired result: Jim and Joan are about equally aggressive.")
   (compare 'Jim5 'Joan5 'aggressive)
   (compare 'Jim4 'Joan4 'aggressive)
   )

   (my-print "P5 loaded.")

    

    

   ; File P6.ambiguous diagnostic behavior
   ; Purpose Stereotypes affect trait ratings in presence of
   ; ambiguous diagnostic behavior, through disambiguating the behavior
   ;
   ;
   ; Programmer: Paul Thagard
   ; Created : October26, 1994

   (print "")
   (my-print "Loading P6")

   ; condition 1: Construction worker expected to be aggressive.
   (defun 6a ()
   (clear-net)
   (observed 'Jim6 'construction-worker)
   ; (observed 'Jim6 'hit-someone)
   (associate 'hit-someone 'punch-adult)
   (associate 'hit-someone 'spank-child)
   (associate 'construction-worker 'aggressive)
   (associate 'construction-worker 'punch-adult )
   (associate 'construction-worker 'spank-child)
   (associate 'punch-adult 'aggressive 3)
   (associate 'spank-child 'aggressive)
   (imp-run 'Jim6)
   )

   ; condition 2: Housewife is not aggressive.
   (defun 6b ()
   (clear-net)
   (observed 'Joan6 'housewife)
   ; (observed 'Joan6 'hit-someone)
   (associate 'housewife 'aggressive -1)
   (associate 'hit-someone 'punch-adult)
   (associate 'hit-someone 'spank-child)
   (associate 'housewife 'punch-adult -1)
   (associate 'housewife 'spank-child)
   (associate 'punch-adult 'aggressive 3)
   (associate 'spank-child 'aggressive)
   (imp-run 'Joan6)
   )

   (defun p6 ()
   (6a) (6b)
   (my-print "P6 desired result: Jim is more aggressive than Joan.")
   (compare 'Jim6 'Joan6 'aggressive)
   (compare 'Jim5 'Joan5 'aggressive)
   (compare 'Jim4 'Joan4 'aggressive)
   )

    

   (defun locksley () (P4) (p5) (p6))

    

   ; File P7.contrast effects
   ; Purpose Stereotypes can provoke contrast effects on trait ratings
   ;
   ;

    

   ; condition 1: black is extra intelligent
   (defun 7a ()
   (clear-net)
   (observed 'Jamal7 'black)
   (observed 'Jamal7 'intelligent)
   (associate 'black 'ghetto-black)
   (associate 'black 'successful-black)
   (associate 'successful-black 'intelligent)
   (associate 'ghetto-black 'successful-black -1)
   (associate 'ghetto-black 'intelligent -1)
   (imp-run 'jamal7)
   )

   ; condition 2: white is intelligent

   (defun 7b ()
   (clear-net)
   (observed 'Hugh7 'white)
   (observed 'Hugh7 'intelligent)
   (associate 'white 'intelligent)
   (imp-run 'hugh7)
   )

   (defun p7 ()
   (7a) (7b)
   (my-print "Desired result: Jamal more intelligent than Hugh.")
   (compare 'jamal7 'hugh7 'intelligent)
   )

   (my-print " P7 loaded.")

    

   ; File P8.behavioral predictions
   ; Purpose Stereotypes can affect behavioral predictions even when they do
   not
   ; affect trait ratings
   ;
   ;

    

   ; condition 1: accountant without individuating information

   (defun 8a ()
   (clear-net)
   (observed 'acc-no-ind 'accountant)
   (associate 'accountant 'nerd)
   (associate 'accountant 'refined)
   (associate 'accountant 'up-mid-class)
   (associate 'accountant 'aggressive -1)
   (associate 'nerd 'punch -1)
   (associate 'refined 'punch -1)
   (associate 'up-mid-class 'punch -1)
   (associate 'aggressive 'punch)
   (imp-run 'acc-no-ind)
   )

   ; condition 2: construction worker without individuating information

   (defun 8b ()
   (clear-net)
   (observed 'con-work-no-ind 'construction-worker)
   (associate 'construction-worker 'unrefined)
   (associate 'construction-worker 'strong)
   (associate 'construction-worker 'working-class)
   (associate 'construction-worker 'aggressive)
   (associate 'strong 'punch)
   (associate 'unrefined 'punch)
   (associate 'working-class 'punch)
   (associate 'aggressive 'punch)
   (imp-run 'con-work-no-ind)
   )

   ; condition 3: accountant walked away
   (defun 8c ()
   (clear-net)
   (observed 'acc-walked 'accountant)
   (observed 'acc-walked 'walked-away)
   (associate 'walked-away 'aggressive -3)
   (associate 'accountant 'nerd)
   (associate 'accountant 'refined)
   (associate 'accountant 'up-mid-class)
   (associate 'accountant 'aggressive -1)
   (associate 'nerd 'punch -1)
   (associate 'refined 'punch -1)
   (associate 'up-mid-class 'punch -1)
   (associate 'aggressive 'punch)
   (imp-run 'acc-walked)
   )
   ; condition 4: construction worker walked away

   (defun 8d ()
   (clear-net)
   (observed 'con-work-walked 'construction-worker)
   (observed 'con-work-walked 'walked-away)
   (associate 'walked-away 'aggressive -3)
   (associate 'construction-worker 'unrefined)
   (associate 'construction-worker 'strong)
   (associate 'construction-worker 'working-class)
   (associate 'construction-worker 'aggressive)
   (associate 'strong 'punch)
   (associate 'unrefined 'punch)
   (associate 'working-class 'punch)
   (associate 'aggressive 'punch)
   (imp-run 'con-work-walked)
   )

   (defun p8 ()
   (8a) (8b) (8c) (8d)
   (my-print "P8 desired result: con-work-no-ind more aggressive than
   acc-no-ind.")
   (compare 'con-work-no-ind 'acc-no-ind 'aggressive)
   (my-print "Desired result: con-work-no-ind more punch than acc-no-ind ")
   (compare 'con-work-no-ind 'acc-no-ind 'punch)
   (my-print "Desired result: con-work-no-ind equally aggressive as
   acc-walked ")
   (compare 'con-work-walked 'acc-walked 'aggressive)
   (my-print "Desired result: con-work-walked more punch than acc-walked. ")
   (compare 'con-work-walked 'acc-walked 'punch)

   )

    

   ; File P9.uncertain info
   ; Purpose Stereotypes sometimes affect impressions in the presence of
   uncertain
   ; information
   ;
   ;
   (my-print "Loading P9")

   ; condition 1: black gets up

   (defun 9a ()
   (clear-net)
   (observed 'Jamal9 'black)
   (observed 'Jamal9 'gets-up)
   (associate 'black 'irresponsible)
   (associate 'irresponsible 'walk-out-no-pay)

   (imp-run 'jamal9)
   )

   ; condition 2: white gets up

   (defun 9b ()
   (clear-net)
   (observed 'Hugh9 'white)
   (observed 'Hugh9 'gets-up)
   (associate 'white 'responsible)
   (associate 'responsible 'walk-out-no-pay -1)
   (imp-run 'hugh9)
   )

   (defun p9 ()
   (9a) (9b)
   (my-print "Desired result: Jamal more walk away than Hugh.")
   (compare 'jamal9 'hugh9 'walk-out-no-pay)
   )

   ; File P10.irrelevant
   ; Purpose Stereotypes affect impressions in the presence of truly
   irrelevant
   ; information
   ;
   ;

    

   ; condition 1: black gets up

   (defun 10a ()
   (clear-net)
   (observed 'Jamal10a 'black)
   (observed 'Jamal10a 'gets-up)
   (associate 'black 'irresponsible)
   (associate 'irresponsible 'walk-out-no-pay)

   (imp-run 'jamal10a)
   )

   ; condition 2: black gets up + more info
   (defun 10b ()
   (clear-net)
   (observed 'Jamal10b 'black)
   (observed 'Jamal10b 'gets-up)
   (observed 'Jamal10b 'short-hair)
   (observed 'Jamal10b 'had-pizza)
   (associate 'black 'irresponsible)
   (associate 'irresponsible 'walk-out-no-pay)

   (imp-run 'jamal10b)
   )

   (defun p10 ()
   (10a) (10b)
   (my-print "Desired result: Jamal10a same as Jamal10b.")
   (compare 'jamal10a 'Jamal10b 'walk-out-no-pay)
   )

   (my-print "P10 loaded.")

    

   ; File P11.dilution
   ; Purpose Nondiagnostic but pseudo-relevant information can eliminate or
   ; dilute the effects of stereotypes
   ;
   ;

   ; condition 1: day person has self-control

   (defun 11a ()
   (clear-net)
   (observed 'Jack11a 'day-person)
   (associate 'day-person 'self-control)
   (imp-run 'Jack11a)
   )

   ; condition 2: night person lacks self-control

   (defun 11b ()
   (clear-net)
   (observed 'Jane11b 'night-person)
   (associate 'night-person 'self-control -1)
   (imp-run 'Jane11b)
   )

   ; condition 3: day-person has 5 other features

   (defun 11c ()
   (clear-net)
   (observed 'Jack11c 'day-person)
   (associate 'day-person 'self-control)
   ;(observed 'Jack11c 'IQ118)
   ;(observed 'Jack11c 'Father-lawyer)
   ;(observed 'Jack11c 'Mother-nurse)
   ;(observed 'Jack11c 'two-friends)
   ;(observed 'Jack11c 'does-well-in-college)
   (associate 'IQ118 'ordinary-person)
   (associate 'Father-lawyer 'ordinary-person)
   (associate 'Mother-nurse 'ordinary-person)
   (associate 'two-friends 'ordinary-person)
   (associate 'does-well-in-college 'ordinary-person)
   (associate 'ordinary-person 'day-person -1)
   (imp-run 'Jack11c)
   )

   ; condition 4: night person has 5 other features

   (defun 11d ()
   (clear-net)
   (observed 'Jane11d 'night-person)
   (associate 'night-person 'self-control -1)
   ;(observed 'Jane11d 'IQ118)
   ;(observed 'Jane11d 'Father-lawyer)
   ;(observed 'Jane11d 'Mother-nurse)
   ;(observed 'Jane11d 'two-friends)
   ;(observed 'Jane11d 'does-well-in-college)
   (associate 'IQ118 'ordinary-person)
   (associate 'Father-lawyer 'ordinary-person)
   (associate 'Mother-nurse 'ordinary-person)
   (associate 'two-friends 'ordinary-person)
   (associate 'does-well-in-college 'ordinary-person)
   (associate 'ordinary-person 'night-person -1)
   (imp-run 'Jane11d)
   )

    

   (defun p11 ()
   (11a) (11b) (11c) (11d)
   (my-print "Desired result: Jack11a more assertive than Jane11b.")
   (compare 'jack11a 'jane11b 'self-control)
   (my-print "Desired result: Jack11c not more assertive than Jane11d.")
   (compare 'jack11c 'jane11d 'self-control)
   )

   ; File P12.multiple
   ; Purpose Stereotypes can influence the activation of subtypes of another
   ; stereotype
   ;
   ;

   ; condition 1: feminist
   (defun 12a ()
   (clear-net)
   (observed 'Linda1 'feminist)
   (associate 'feminist 'highly-educated)
   (imp-run 'Linda1)
   )

   ; condition 2: bank teller

   (defun 12b ()
   (clear-net)
   (observed 'Linda2 'bank-teller)
   (associate 'bank-teller 'highly-educated -1)
   (imp-run 'Linda2)
   )

   ; condition 3: feminist bank teller

   (defun 12c ()
   (clear-net)
   (observed 'Linda3 'bank-teller)
   (associate 'bank-teller 'highly-educated -1)
   (observed 'Linda3 'feminist)
   (associate 'feminist 'highly-educated)
   (imp-run 'Linda3)
   )
   (defun p12 ()
   (12a) (12b) (12c)
   (my-print "Desired result: Linda1 more educated than Linda2.")
   (compare 'Linda1 'Linda2 'highly-educated)
   (my-print "Desired result: Linda 3 in between other two.")
   (compare 'Linda2 'Linda3 'highly-educated)
   )

   (print "P12 loaded.")

    

   ; File P13.primacy
   ; Purpose: primacy effects
   ;
   ;
   ; Programmer: Paul Thagard
   ; Created : July, 1995

   ; To run incrementally, run 200 timesteps rather than stopping after
   ; asymptote.

   (setq stop-setted? nil)
   (setq *max-times* 400)

   ; condition 1: simultaneous presentation
   (defun 13a ()
   (clear-net)
   (clear-props 'jack1)
   (observed 'Jack1 'intelligent)
   (observed 'Jack1 'stubborn)
   (associate 'intelligent 'effective 2)
   (associate 'intelligent 'arrogant)
   (associate 'effective 'likeable)
   (associate 'arrogant 'unlikeable )
   (associate 'likeable 'unlikeable -2)
   (associate 'stubborn 'obstructive 2)
   (associate 'stubborn 'persistent)
   (associate 'persistent 'likeable)
   (associate 'obstructive 'unlikeable)
   (imp-run 'jack1)
   (pl 'jack1)
   )

   ; condition 2: intelligent, then stubborn

   (defun 13b ()
   (clear-net)
   (clear-props 'jack2)
   (observed 'Jack2 'intelligent)
   ; stubborn delayed

   (associate 'intelligent 'effective 2)
   (associate 'intelligent 'arrogant)
   (associate 'effective 'likeable)
   (associate 'arrogant 'unlikeable )
   (associate 'likeable 'unlikeable -2)
   (imp-run 'jack2)
   (pl 'jack2)
   (setq *asymptoted-units* nil)
   (observed 'Jack2 'stubborn)

   (associate 'stubborn 'obstructive 2)
   (associate 'stubborn 'persistent)
   (associate 'persistent 'likeable)
   (associate 'obstructive 'likeable)

   (imp-run 'jack2)
   (pl 'jack2)

   )

   ; condition 3: stubborn, then intelligent

   (defun 13c ()
   (clear-net)
   (clear-props 'jack3)
   ; intelligent delayed
   (observed 'Jack3 'stubborn)
   (associate 'likeable 'unlikeable -2)
   (associate 'stubborn 'obstructive 2)
   (associate 'stubborn 'persistent)
   (associate 'persistent 'likeable)
   (associate 'obstructive 'unlikeable )
   (imp-run 'jack3)
   (pl 'jack3)
   (setq *asymptoted-units* nil)
   (observed 'Jack3 'intelligent)
   (associate 'intelligent 'effective 2)
   (associate 'intelligent 'arrogant)
   (associate 'effective 'likeable)
   (associate 'arrogant 'unlikeable )

   (imp-run 'jack3)
   (pl 'jack3)

   )

   (defun p13 ()
   (13a) (13b) (13c)
   (my-print "P13 desired result: Jack 1 more likeable than Jack3.")
   (my-print " Jack1 less likeable than Jack2.")
   (compare 'jack1 'jack2 'likeable)
   (compare 'jack1 'jack3 'likeable)
   )

   (my-print "P13 loaded.")

   (my-print "P8 LOADED")
