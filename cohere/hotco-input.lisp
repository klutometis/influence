    

   ; FILE: HOTCO.OJ
   ; PURPOSE: apply emotional coherence to O. J. Simpson case
   ; PROGRAMMER: Paul Thagard
   ; CREATED: 6-21-2000
   ; UPDATED: 7-20-2000
   ; UPDATED: 11-9-2000: everything is an evaluation unit!

   ; UPDATED: 1-22-2002, adding Sahdra's Dimmesdale simulation

    

   ; See [1]http://cogsci.uwaterloo.ca/Articles/Pages/oj.html

   ; ************************************************
   ; Experiment 1, ECHO analysis - cold explanation

   (defun oj1 ()

   (setq *problem* 'oj-cold)
   (clear-net)

   ; Evidence:
   (proposition 'dead "Nicole and Goldman were killed.")
   ; (proposition 'Chicago "OJ left for Chicago.")
   (proposition 'bloody-car "Blood in OJ's car.")
   (proposition 'bloody-glove "Blood on glove found in OJ's house.")
   (proposition 'bloody-sock "Blood on OJ's sock.")
   (proposition 'bloody-gate "Blood on gate at crime scene.")
   ; (proposition 'pre-booked "OJ was prebooked for Chicago.") ; check
   (proposition 'beatings "OJ beat Nicole.")
   (proposition 'EDTA-socks "EDTA was found in blood in socks.")
   ;(proposition 'LAPD-framed-others "LAPD framed other blacks.")
   (proposition 'Fuhrman-lied "Fuhrman denied he was racist.")

   ; Guilty hypotheses:
   (proposition 'OJ-killed "OJ killed Nicole and Goldman. ")
   (proposition 'abusive "OJ was abusive.")

   ; Alternative hypothees
   (proposition 'drug-kill "Drug dealers killed Nicole and Goldman.")
   ; (proposition 'business-trip "OJ went to Chicago on planned business.")
   (proposition 'LAPD-frame "LAPD planted evidence.")
   ; (proposition 'LAPD-racist "LAPD was racist.")

   ; Contradictions:

   ; guilty explanations:
   (explain '(OJ-killed) 'dead)
   ; (explain '(OJ-killed) 'Chicago)
   (explain '(OJ-killed) 'bloody-car)
   (explain '(OJ-killed) 'bloody-glove)
   (explain '(OJ-killed) 'bloody-sock)
   (explain '(OJ-killed) 'bloody-gate)
   (explain '(abusive) 'OJ-killed)
   (explain '(abusive) 'beatings)

   ; Alternative explanations:
   (explain '(drug-kill) 'dead)
   ; (explain '(business-trip) 'Chicago)
   ; (explain '(business-trip) 'pre-booked)
   (explain '(LAPD-frame) 'bloody-car)
   (explain '(LAPD-frame) 'bloody-glove)
   (explain '(LAPD-frame) 'bloody-sock)
   (explain '(LAPD-frame) 'bloody-gate)
   (explain '(LAPD-frame) 'EDTA-socks)
   (explain '(LAPD-racist) 'LAPD-frame)
   ; (explain '(LAPD-racist) 'LAPD-framed-others)
   (explain '(LAPD-frame) 'Fuhrman-lied)

   (data '(dead bloody-gate bloody-car bloody-glove bloody-sock beatings
   EDTA-socks Fuhrman-lied
   )
   )
   ; to implement reasonable doubt, provide presumption of innocence.
   ; Result: OJ deemed innocent if doubt factor is >= -1.4.
   ; (data '((OJ-killed -1.4)))

   (make-competition) ; ECHO.2
   (reset-act *all-units*)
   (eval-cohere)
   )

   ; =================================================
   ; Experiment 2, - emotional coherence
   ; Blacks motivated to like OJ

   (defun oj2 ()

   (setq *problem* 'oj-hot)
   (clear-net)
   (hot)

   ; Evidence:
   (proposition 'dead "Nicole and Goldman were killed.")
   ; (proposition 'Chicago "OJ left for Chicago.")
   (proposition 'bloody-car "Blood in OJ's car.")
   (proposition 'bloody-glove "Blood on glove found in OJ's house.")
   (proposition 'bloody-sock "Blood on OJ's sock.")
   (proposition 'bloody-gate "Blood on gate at crime scene.")
   ; (proposition 'pre-booked "OJ was prebooked for Chicago.") ; check
   (proposition 'beatings "OJ beat Nicole.")
   (proposition 'EDTA-socks "EDTA was found in blood in socks.")
   ;(proposition 'LAPD-framed-others "LAPD framed other blacks.")
   (proposition 'Fuhrman-lied "Fuhrman denied he was racist.")

   ; Guilty hypotheses:
   (proposition 'OJ-killed "OJ killed Nicole and Goldman. ")
   (proposition 'abusive "OJ was abusive.")

   ; Alternative hypothees
   (proposition 'drug-kill "Drug dealers killed Nicole and Goldman.")
   ; (proposition 'business-trip "OJ went to Chicago on planned business.")
   (proposition 'LAPD-frame "LAPD planted evidence.")
   ; (proposition 'LAPD-racist "LAPD was racist.")

   ; Contradictions:

   ; guilty explanations:
   (explain '(OJ-killed) 'dead)
   ; (explain '(OJ-killed) 'Chicago)
   (explain '(OJ-killed) 'bloody-car)
   (explain '(OJ-killed) 'bloody-glove)
   (explain '(OJ-killed) 'bloody-sock)
   (explain '(OJ-killed) 'bloody-gate)
   (explain '(abusive) 'OJ-killed)
   (explain '(abusive) 'beatings)

   ; Alternative explanations:
   (explain '(drug-kill) 'dead)
   ; (explain '(business-trip) 'Chicago)
   ; (explain '(business-trip) 'pre-booked)
   (explain '(LAPD-frame) 'bloody-car)
   (explain '(LAPD-frame) 'bloody-glove)
   (explain '(LAPD-frame) 'bloody-sock)
   (explain '(LAPD-frame) 'bloody-gate)
   (explain '(LAPD-frame) 'EDTA-socks)
   (explain '(LAPD-racist) 'LAPD-frame)
   ; (explain '(LAPD-racist) 'LAPD-framed-others)
   (explain '(LAPD-frame) 'Fuhrman-lied)

   (data '(dead bloody-gate bloody-car bloody-glove bloody-sock beatings
   EDTA-socks Fuhrman-lied
   )
   )
   ; to implement reasonable doubt, provide presumption of innocence.
   ; Result: OJ deemed innocent if doubt factor is >= -.1,
   ; when valence of OJ-good is only 2.
   ; (data '((OJ-killed -.1)))

   (make-competition) ; ECHO.2

   ; HOTCO
   ; emotional inputs for black jurors
   (valence-unit 'valence-special
   '((OJ-good 1)
   (LAPD-good -1)
   )
   )

   ; for HOTCO 2, note the evaluation units

   (setf *evaluation-units* *all-units*)

   ; associations between evaluation units and hypotheses

   (associate 'oj-good 'oj-killed -1)
   (associate 'lapd-good 'lapd-frame -1)
   (associate 'oj-good 'abusive -1)
   (associate 'lapd-good 'lapd-racist -1)

   (reset-act '(oj-killed lapd-frame oj-good lapd-good drug-kill))
   (eval-cohere )
   (pls)
   (show-valence)
   )

   ; =================================================
   ; Experiment 3, - emotional coherence hot, but without
   ; the LAPD part of the case. Motivation not enough to acquit?

   (defun oj3 ()

   (setq *problem* 'oj-hot-only)
   (clear-net)
   (hot)

   ; Evidence:
   (proposition 'dead "Nicole and Goldman were killed.")
   ; (proposition 'Chicago "OJ left for Chicago.")
   (proposition 'bloody-car "Blood in OJ's car.")
   (proposition 'bloody-glove "Blood on glove found in OJ's house.")
   (proposition 'bloody-sock "Blood on OJ's sock.")
   (proposition 'bloody-gate "Blood on gate at crime scene.")
   ; (proposition 'pre-booked "OJ was prebooked for Chicago.") ; check
   (proposition 'beatings "OJ beat Nicole.")
   (proposition 'EDTA-socks "EDTA was found in blood in socks.")
   ;(proposition 'LAPD-framed-others "LAPD framed other blacks.")
   (proposition 'Fuhrman-racist "Fuhrman made racist comments.")

   ; Guilty hypotheses:
   (proposition 'OJ-killed "OJ killed Nicole and Goldman. ")
   (proposition 'abusive "OJ was abusive.")

   ; Alternative hypothees
   (proposition 'drug-kill "Drug dealers killed Nicole and Goldman.")
   ; (proposition 'business-trip "OJ went to Chicago on planned business.")
   (proposition 'LAPD-frame "LAPD planted evidence.")
   (proposition 'LAPD-racist "LAPD was racist.")

   ; Contradictions:

   ; guilty explanations:
   (explain '(OJ-killed) 'dead)
   ; (explain '(OJ-killed) 'Chicago)
   (explain '(OJ-killed) 'bloody-car)
   (explain '(OJ-killed) 'bloody-glove)
   (explain '(OJ-killed) 'bloody-sock)
   (explain '(OJ-killed) 'bloody-gate)
   (explain '(abusive) 'OJ-killed)
   (explain '(abusive) 'beatings)

   ; Alternative explanations:
   (explain '(drug-kill) 'dead)
   ; (explain '(business-trip) 'Chicago)
   ; (explain '(business-trip) 'pre-booked)
   ; (explain '(LAPD-frame) 'bloody-car)
   ; (explain '(LAPD-frame) 'bloody-glove)
   ; (explain '(LAPD-frame) 'bloody-sock)
   ; (explain '(LAPD-frame) 'bloody-gate)
   ; (explain '(LAPD-frame) 'EDTA-socks)
   ; (explain '(LAPD-racist) 'LAPD-frame)
   ; (explain '(LAPD-racist) 'LAPD-framed-others)
   ; (explain '(LAPD-racist) 'Fuhrman-racist)

   (data '(dead bloody-gate bloody-car bloody-glove bloody-sock beatings
   EDTA-socks Fuhrman-racist
   )
   )

   (make-competition) ; ECHO.2

   ; HOTCO
   ; emotional inputs for black jurors
   (valence-unit 'valence-special
   '((OJ-good 1)
   (LAPD-good -1)
   )
   )

   ; for HOTCO 2, note the evaluation units

   (setf *evaluation-units* *all-units*)

   ; associations between evaluation units and hypotheses

   (associate 'oj-good 'oj-killed -1)
   (associate 'lapd-good 'lapd-frame -1)
   (associate 'oj-good 'abusive -1)
   (associate 'lapd-good 'lapd-racist -1)

   (reset-act '(oj-killed lapd-frame oj-good lapd-good drug-kill))
   (eval-cohere )
   (pls)
   (show-valence)
   )
   ; =================================================
   ; Experiment 4, - emotional coherence + reasonable doubt

   (defun oj4 ()

   (setq *problem* 'oj-hot-doubt)
   (clear-net)
   (hot)

   ; Evidence:
   (proposition 'dead "Nicole and Goldman were killed.")
   ; (proposition 'Chicago "OJ left for Chicago.")
   (proposition 'bloody-car "Blood in OJ's car.")
   (proposition 'bloody-glove "Blood on glove found in OJ's house.")
   (proposition 'bloody-sock "Blood on OJ's sock.")
   (proposition 'bloody-gate "Blood on gate at crime scene.")
   ; (proposition 'pre-booked "OJ was prebooked for Chicago.") ; check
   (proposition 'beatings "OJ beat Nicole.")
   (proposition 'EDTA-socks "EDTA was found in blood in socks.")
   ;(proposition 'LAPD-framed-others "LAPD framed other blacks.")
   (proposition 'Fuhrman-lied "Fuhrman denied he was racist.")

   ; Guilty hypotheses:
   (proposition 'OJ-killed "OJ killed Nicole and Goldman. ")
   (proposition 'abusive "OJ was abusive.")

   ; Alternative hypothees
   (proposition 'drug-kill "Drug dealers killed Nicole and Goldman.")
   ; (proposition 'business-trip "OJ went to Chicago on planned business.")
   (proposition 'LAPD-frame "LAPD planted evidence.")
   ; (proposition 'LAPD-racist "LAPD was racist.")

   ; Contradictions:

   ; guilty explanations:
   (explain '(OJ-killed) 'dead)
   ; (explain '(OJ-killed) 'Chicago)
   (explain '(OJ-killed) 'bloody-car)
   (explain '(OJ-killed) 'bloody-glove)
   (explain '(OJ-killed) 'bloody-sock)
   (explain '(OJ-killed) 'bloody-gate)
   (explain '(abusive) 'OJ-killed)
   (explain '(abusive) 'beatings)

   ; Alternative explanations:
   (explain '(drug-kill) 'dead)
   ; (explain '(business-trip) 'Chicago)
   ; (explain '(business-trip) 'pre-booked)
   (explain '(LAPD-frame) 'bloody-car)
   (explain '(LAPD-frame) 'bloody-glove)
   (explain '(LAPD-frame) 'bloody-sock)
   (explain '(LAPD-frame) 'bloody-gate)
   (explain '(LAPD-frame) 'EDTA-socks)
   (explain '(LAPD-racist) 'LAPD-frame)
   ; (explain '(LAPD-racist) 'LAPD-framed-others)
   (explain '(LAPD-frame) 'Fuhrman-lied)

   (data '(dead bloody-gate bloody-car bloody-glove bloody-sock beatings
   EDTA-socks Fuhrman-lied acquit-innocent
   )
   )

   (make-competition) ; ECHO.2

   ; HOTCO
   ; emotional inputs for black jurors
   (valence-unit 'valence-special
   '((OJ-good 1)
   (LAPD-good -1)
   (acquit-innocent 1) ; valence AND data needed
   )
   )

   ; for HOTCO 2, note the evaluation units

   (setf *evaluation-units* *all-units*)

   ; associations between evaluation units and hypotheses

   (associate 'oj-good 'oj-killed -1)
   (associate 'acquit-innocent 'oj-killed -1)
   (associate 'lapd-good 'lapd-frame -1)
   (associate 'oj-good 'abusive -1)
   (associate 'lapd-good 'lapd-racist -1)

   (reset-act '(oj-killed lapd-frame oj-good lapd-good drug-kill))
   (eval-cohere )
   (pls)
   (show-valence)
   )

    

   ; ===============================
   ; Sinclair and Kunda, motivated inhibition

   (defun k1 () ; no input about evaluation - pos-black=neg-black
   ; note: this is too cognitive. Hot influence should decide.
   (hot)
   (clear-net)
   (observed 'manager 'black)
   (associate 'bad-eval 'bad-inac-eval)
   (associate 'bad-inac-eval 'incomp-man)
   (associate 'incomp-man 'neg-black)
   (associate 'good-eval 'good-ac-eval)
   (associate 'good-ac-eval 'comp-man)
   (associate 'comp-man 'pos-black)
   (associate 'megood 'bad-inac-eval)
   (associate 'megood 'good-ac-eval)
   (associate 'bad-eval 'good-eval -1)
   (associate 'bad-inac-eval 'good-ac-eval -1)
   (associate 'incomp-man 'comp-man -1)
   (associate 'pos-black 'neg-black -1)
   (valence-unit 'valence-special ; self-enhancement
   '((megood 1))
   )
   (setf *evaluation-units* *all-units*)
   (reset-act *all-units*)
   (eval-cohere )
   )

   (defun k2 () ; positive evaluation - pos-black > neg-black
   (hot)
   (clear-net)
   (observed 'manager 'black)
   (observed 'manager 'good-eval)
   (associate 'incomp-man 'neg-black)
   (associate 'comp-man 'pos-black)
   (associate 'good-eval 'good-ac-eval)
   (associate 'good-eval 'good-inac-eval)
   (associate 'good-ac-eval 'comp-man)
   (associate 'good-inac-eval 'incomp-man)
   (associate 'megood 'good-ac-eval)
   (associate 'incomp-man 'comp-man -1)
   (associate 'pos-black 'neg-black -1)
   (valence-unit 'valence-special ; self-enhancement
   '((megood 1))
   )
   (setf *evaluation-units* *all-units*)
   (reset-act *all-units*)
   (eval-cohere )
   )

   (defun k3 () ; bad evaluation - pos-black<neg-black
   (hot)
   (clear-net)
   (observed 'manager 'black)
   (observed 'manager 'bad-eval)
   (associate 'bad-eval 'bad-inac-eval)
   (associate 'bad-eval 'bad-ac-eval)
   (associate 'bad-inac-eval 'incomp-man)
   (associate 'bad-ac-eval 'comp-man)
   (associate 'incomp-man 'neg-black)
   (associate 'comp-man 'pos-black)
   (associate 'megood 'bad-inac-eval)
   (associate 'incomp-man 'comp-man -1)
   (associate 'pos-black 'neg-black -1)
   (associate 'bad-ac-eval 'bad-inac-eval -1)
   (valence-unit 'valence-special ; self-enhancement
   '((megood 1))
   )
   (setf *evaluation-units* *all-units*)
   (reset-act *all-units*)
   (eval-cohere )
   )

    

   ; ===========================================
   ; Simulate Westen & Feit

   (defun w1 () ; no bias

   (setq *problem* 'neutral)
   (clear-net)
   (hot)

   ; Evidence:
   (proposition 'accusation "Willey says Clinton harassed.")
   (proposition 'denial "Clinton says no harassment.")

   ; Guilty hypotheses:
   (proposition 'harassment "Clinton harassed Willey. ")
   (proposition 'no-harassment "Clinton did not harass.")
   (contradict 'harassment 'no-harassment)

   ; explanations:
   (explain '(harassment) 'accusation)
   (explain '(no-harassment) 'denial)

   ; data
   (data '(accusation denial))

   ; HOTCO
   ; emotional inputs for black jurors
   (valence-unit 'valence-special
   '((democrats 0)
   (republicans 0)
   )
   )
   ; evaluation units
   ;(valence-unit 'democrats
   ; '((clinton 1)) ; node means clinton is good
   ;)

   ;(valence-unit 'republicans
   ; '((clinton -1) );
   ;)

   (setf *evaluation-units* *all-units*)

   ; associations between evaluation units and hypotheses

   ;(associate 'harassment 'democrats -1)
   ;(associate 'harassment 'republicans 1)

   (make-competition) ; ECHO.2
   (reset-act '(republicans democrats harassment no-harassment
   accusation denial))
   (eval-cohere)
   (pls)
   (show-valence)
   )

   (defun w2 () ; bias to republicans

   (setq *problem* 'neutral)
   (clear-net)
   (hot)

   ; Evidence:
   (proposition 'accusation "Willey says Clinton harassed.")
   (proposition 'denial "Clinton says no harassment.")

   ; Guilty hypotheses:
   (proposition 'harassment "Clinton harassed Willey. ")
   (proposition 'no-harassment "Clinton did not harass.")
   (contradict 'harassment 'no-harassment)

   ; explanations:
   (explain '(harassment) 'accusation)
   (explain '(no-harassment) 'denial)

   ; data
   (data '(accusation denial))

   ; HOTCO
   ; emotional inputs for black jurors
   (valence-unit 'valence-special
   '((democrats -1)
   (republicans 1)
   )
   )

    

   (setf *evaluation-units* *all-units*)

   ; associations between evaluation units and hypotheses

   (associate 'harassment 'democrats -1)
   (associate 'harassment 'republicans 1)

   (make-competition) ; ECHO.2
   (reset-act '(republicans democrats harassment no-harassment
   accusation denial))
   (eval-cohere)
   (pls)
   (show-valence)
   )

   (defun w3 () ; bias to democrats

   (setq *problem* 'pro-democrat)
   (clear-net)
   (hot)

   ; Evidence:
   (proposition 'accusation "Willey says Clinton harassed.")
   (proposition 'denial "Clinton says no harassment.")

   ; Guilty hypotheses:
   (proposition 'harassment "Clinton harassed Willey. ")
   (proposition 'no-harassment "Clinton did not harass.")
   (contradict 'harassment 'no-harassment)

   ; explanations:
   (explain '(harassment) 'accusation)
   (explain '(no-harassment) 'denial)

   ; data
   (data '(accusation denial))

   ; HOTCO
   ; emotional inputs for black jurors
   (valence-unit 'valence-special
   '((democrats 1)
   (republicans -1)
   )
   )

    

   (setf *evaluation-units* *all-units*)

   ; associations between evaluation units and hypotheses

   (associate 'harassment 'democrats -1)
   (associate 'harassment 'republicans 1)

    

   (make-competition) ; ECHO.2
   (reset-act '(republicans democrats harassment no-harassment
   accusation denial))
   (eval-cohere)
   (pls)
   (show-valence)
   )
   (defun w4 () ; bias to democrats, but more evidence
   ; result: 3 pieces of additional evidence needed to overcome bias

   (setq *problem* 'pro-democrat)
   (clear-net)
   (hot)

   ; Evidence:
   (proposition 'accusation "Willey says Clinton harassed.")
   (proposition 'denial "Clinton says no harassment.")

   ; Guilty hypotheses:
   (proposition 'harassment "Clinton harassed Willey. ")
   (proposition 'no-harassment "Clinton did not harass.")
   (proposition 'evidence3 "More evidence 3.")
   (proposition 'evidence4 "More evidence 4.")
   (proposition 'evidence3 "More evidence 5.")
   ; (proposition 'evidence4 "More evidence 6.")
   (contradict 'harassment 'no-harassment)

   ; explanations:
   (explain '(harassment) 'accusation)
   (explain '(harassment) 'evidence3)
   (explain '(harassment) 'evidence4)
   (explain '(harassment) 'evidence5)
   ; (explain '(harassment) 'evidence6)
   (explain '(no-harassment) 'denial)

   ; data
   (data '(accusation denial evidence3 evidence4 evidence5 evidence6))

   ; HOTCO
   ; emotional inputs for black jurors
   (valence-unit 'valence-special
   '((democrats 1)
   (republicans -1)
   )
   )

    

   (setf *evaluation-units* *all-units*)

   ; associations between evaluation units and hypotheses

   (associate 'harassment 'democrats -1)
   (associate 'harassment 'republicans 1)

    

   (make-competition) ; ECHO.2
   (reset-act '(republicans democrats harassment no-harassment
   accusation denial))
   (eval-cohere)
   (pls)
   (show-valence)
   )

    

   (defun t1 ()
   ; test: result is H2 beats H1
   (setq *problem* 'test)
   (clear-net)

   ; Evidence:
   (proposition 'E1 "E1")
   (proposition 'H1 "H1")
   (proposition 'H2 "H2")
   (proposition 'H3 "H3")
   (proposition 'H4 "H4")

   (explain '(H1) 'E1)
   (explain '(H2) 'E1)
   (contradict 'H3 'H2)
   ; (contradict 'H4 'H1)

   (data '(E1))

   (make-competition) ; ECHO.2
   (reset-act *all-units*)
   (eval-cohere)
   )

   ======================================

    

   ; FILE: Self-deception.lisp
   ; PURPOSE: Apply emotional coherence and approach-avoidance concepts
   ; to model Dimmesdale's self-deception
   ; PROGRAMMER: Baljinder Sahdra
   ; CREATED: 3-17-2001
   ; UPDATED: 3-28-2001

   ;
   ***************************************************************************
   ; Experiment 1: Cold explanation -"the impartial observer test"

   (defun cg-cold () ;clergyman-cold
   (clear-net)
   (setq *emote* nil)
   (setf *silent-run?* t)

   ; Observed:
   (observed 'clergyman 'performDuty)
   (observed 'clergyman 'hadAffair)
   (observed 'clergyman 'feelingsForGirls)
   (observed 'clergyman 'relationsLady)
   (observed 'clergyman 'ChillingworhtKnows)
   (observed 'clergyman 'righteous)
   (observed 'clergyman 'runawayplan)
   (observed 'clergyman 'laughDeacon)
   (observed 'clergyman 'blasphemousWords)
   (observed 'clergyman 'wickedWords)
   (observed 'clergyman 'atheism)
   (observed 'clergyman 'immortalityOfSoul)

   ; Associate:
   (associate 'performDuty 'hypocriticalButSelfless)
   (associate 'hypocritical 'hypocriticalButSelfless)
   (associate 'hadAffair 'sinned)
   (associate 'feelingsForGirls 'sinned)
   (associate 'relationLady 'sinned)

   (associate 'sinned 'preachAndSin )
   (associate 'preachAndSin 'hypocritical)
   (associate 'ChillingworhtKnows 'peopleWillKnow)
   (associate 'ChillingworhtKnows 'righteous -1)
   (associate 'peopleWillKnow 'righteous -1)
   (associate 'righteous 'hypocriticalRighteousSelfish)
   (associate 'hypocritical 'hypocriticalRighteousSelfish)

   (associate 'laughDeacon 'blasphemous)
   (associate 'blasphemousWords 'blasphemous)
   (associate 'wickedWords 'blasphemous)
   (associate 'atheism 'blasphemous)
   (associate 'immortalityOfSoul 'blasphemous)
   (associate 'immortalityOfSoul 'atheism)
   (associate 'runawayplan 'rejectSuffering)
   (associate 'blasphemous 'rejectSuffering)
   (associate 'runawayplan 'selfish)
   (associate 'hypocriticalRighteousSelfish 'selfish)
   (associate 'hypocriticalRighteousSelfish 'cannotPreach)
   (associate 'cannotPreach 'badfaith)
   (associate 'badfaith 'badClergyman)
   (associate 'badClergyman 'damnation)

   (associate 'hypocriticalButSelfless 'canPreach)
   (associate 'canPreach 'goodfaith)
   (associate 'goodfaith 'goodClergyman)
   (associate 'goodClergyman 'redemption)

   (associate 'redemption 'damnation -1)
   (associate 'goodClergyman 'badClergyman -1)
   (associate 'goodfaith 'badfaith -1)
   (associate 'canPreach 'cannotPreach -1)
   (associate 'hypocriticalRighteousSelfish 'hypocriticalButSelfless -1)
   (associate 'redemption 'sinned -1)
   (associate 'damnation 'sinned)

   (setf *evaluation-units* nil)
   (reset-act '(redemption damnation goodClergyman badClergyman
   goodfaith badfaith canPreach cannotPreach
   hypocriticalButSelfless hypocriticalRighteousSelfish
   performDuty hypocritical selfish righteous rejectSuffering
   blasphemous preachAndSin sinned peopleWillKnow))
   (eval-cohere)
   (print-propns)
   )

   ;
   ==================================================================================
   ; Experiment 2, - Hot explanation - emotional coherence, and
   approach/avoid goals

   (defun cg-hot () ;clergyman-hot
   (clear-net)
   (hot)
   (setq *goal-units* nil)
   (setq *emotion-units* nil)
   (setf *silent-run?* t)

   ; Approach-Avoid
   (approach 'redemption 1)
   (avoid 'damnation 1)

   ; Like-Dislike
   (dislike 'badclergyman .5)
   (like 'goodClergyman .5)

   (dislike 'badfaith .5)
   (like 'goodfaith .5)

   (dislike 'cannotPreach .5)
   (like 'canPreach .5)

   (dislike 'hypocritical .5)
   (dislike 'selfish .5)
   (dislike 'blasphemous .5)

   (dislike 'rejectSuffering .5)
   (dislike 'sinned .5)

   (like 'performDuty .5)
   (dislike 'preachAndSin .5)
   (dislike 'runawayplan .5)

   (dislike 'hadAffair .5)
   (dislike 'feelingsForGirls .5)
   (dislike 'relationsLady .5)

   (dislike 'ChillingworhtKnows .5)
   (dislike 'peopleWillKnow .5)

   ; Observed:
   (observed 'clergyman 'performDuty)
   (observed 'clergyman 'hadAffair)
   (observed 'clergyman 'feelingsForGirls)
   (observed 'clergyman 'relationsLady)
   (observed 'clergyman 'ChillingworhtKnows)
   (observed 'clergyman 'righteous)
   (observed 'clergyman 'runawayplan)
   (observed 'clergyman 'laughDeacon)
   (observed 'clergyman 'blasphemousWords)
   (observed 'clergyman 'wickedWords)
   (observed 'clergyman 'atheism)
   (observed 'clergyman 'immortalityOfSoul)

   ; Associate:
   (associate 'performDuty 'hypocriticalButSelfless)
   (associate 'hypocritical 'hypocriticalButSelfless)
   (associate 'hadAffair 'sinned)
   (associate 'feelingsForGirls 'sinned)
   (associate 'relationLady 'sinned)

   (associate 'sinned 'preachAndSin )
   (associate 'preachAndSin 'hypocritical)
   (associate 'ChillingworhtKnows 'peopleWillKnow)
   (associate 'ChillingworhtKnows 'righteous -1)
   (associate 'peopleWillKnow 'righteous -1)
   (associate 'righteous 'hypocriticalRighteousSelfish)
   (associate 'hypocritical 'hypocriticalRighteousSelfish)

   (associate 'laughDeacon 'blasphemous)
   (associate 'blasphemousWords 'blasphemous)
   (associate 'wickedWords 'blasphemous)
   (associate 'atheism 'blasphemous)
   (associate 'immortalityOfSoul 'blasphemous)
   (associate 'immortalityOfSoul 'atheism)

   (associate 'runawayplan 'rejectSuffering)
   (associate 'blasphemous 'rejectSuffering)
   (associate 'runawayplan 'selfish)
   (associate 'hypocriticalRighteousSelfish 'selfish)
   (associate 'hypocriticalRighteousSelfish 'cannotPreach)
   (associate 'cannotPreach 'badfaith)
   (associate 'badfaith 'badClergyman)
   (associate 'badClergyman 'damnation)

   (associate 'hypocriticalButSelfless 'canPreach)
   (associate 'canPreach 'goodfaith)
   (associate 'goodfaith 'goodClergyman)
   (associate 'goodClergyman 'redemption)

   (associate 'redemption 'damnation -1)
   (associate 'goodClergyman 'badClergyman -1)
   (associate 'goodfaith 'badfaith -1)
   (associate 'canPreach 'cannotPreach -1)
   (associate 'hypocriticalRighteousSelfish 'hypocriticalButSelfless -1)
   (associate 'redemption 'sinned -1)
   (associate 'damnation 'sinned)

   (setf *evaluation-units* *all-units*)
   (reset-act '(redemption damnation goodClergyman badClergyman
   goodfaith badfaith canPreach cannotPreach
   hypocriticalButSelfless hypocriticalRighteousSelfish
   performDuty hypocritical selfish righteous rejectSuffering
   blasphemous preachAndSin sinned peopleWillKnow))
   (eval-cohere)
   (print-propns)
   )

    

    

    

    

    

References

   Visible links
   1. http://cogsci.uwaterloo.ca/Articles/Pages/oj.html
