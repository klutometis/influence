   ; FILE: lavois.2
   ; PURPOSE: test explanatory coherence on oxygen vs. phlogiston
   ; input file for ECHO2. Original is chem.l.
   ; PROGRAMMER: Paul Thagard
   ; CREATED: 8-23-89
   ; UPDATED: for cohere, 5-4-95, pt

   ; *************************************************

   (defun ll () (load "Peirce HD:LISP:COHERE:lavoisier"))

    

   ; ************************************************
   ; Experiment 1, based on Lavoisier's Reflexions <1783>.
   ; Page numbers are to this work.

   (defun lavoisier ()

   (setq *problem* 'Lavoisier)
   (clear-net)

   ; Evidence:
   (proposition 'E1 "In combustion, heat and light are given off.") ; 624,
   646
   (proposition 'E2 "Inflammability is transmittable from one body to
   another.") ; 625
   (proposition 'E3 "Combustion only occurs in the presence of pure air.")
   ;646
   (proposition 'E4 "Increase in weight of a burned body is exactly equal to
   weight of air absorbed. ") ; 646
   ; Note quantitative nature of E4, etc.
   (proposition 'E5 "Metals undergo calcination.") ; 624
   (proposition 'E6 "In calcination, bodies increase weight.") ; 628
   (proposition 'E7 "In calcination, volume of air diminishes.") ; 628
   (proposition 'E8 "In reduction, effervescence appears.") ; 628

    

    

   ; Oxygen hypotheses:
   (proposition 'OH1 "Pure air contains oxygen principle.") ; 625
   (proposition 'OH2 "Pure air contains matter of fire and heat (MFH).") ;
   625
   (proposition 'OH3 "In combustion, oxygen from the air combines with the
   burning body.")
   (proposition 'OH4 "Oxygen has weight.")
   (proposition 'OH5 "In calcination, metals add oxygen to become calxes. ")
   ; 629
   (proposition 'OH6 "In reduction, oxygen is given off.") ; 628

   ; Phlogiston hypotheses:
   (proposition 'PH1 "Combustible bodies contain phlogiston.") ; 624
   (proposition 'PH2 "Combustible bodies contain matter of heat.") ; 652
   (proposition 'PH3 "In combustion, phlogiston is given off.") ; 624
   (proposition 'PH4 "Phlogiston can pass from one body to another.") ; 625
   (proposition 'PH5 "Metals contain phlogiston.") ; 624
   (proposition 'PH6 "In calcination, phlogiston is given off.") ; 624

   ; Contradictions:

   ; (contradict 'PH3 'OH3)
   ; (contradict 'PH6 'OH5) ; 652

    

   ; Oxygen explanations:
   (explain '(OH1 OH2 OH3) 'E1)
   ; E2?
   (explain '(OH1 OH3) 'E3)
   (explain '(OH1 OH3 OH4) 'E4)
   (explain '(OH1 OH5) 'E5)
   (explain '(OH1 OH4 OH5) 'E6)
   (explain '(OH1 OH5) 'E7)
   (explain '(OH1 OH6) 'E8)

   ; Phlogiston explanations:
   (explain '(PH1 PH2 PH3) 'E1)
   (explain '(PH1 PH3 PH4) 'E2)
   (explain '(PH5 PH6) 'E5)

   (data '(E1 E2 E3 E4 E5 E6 E7 E8 ))

   (make-competition) ; ECHO.2
   (reset-act *all-units*)
   (eval-cohere)
   )

    

   ; FILE: reality
   ; PURPOSE: metaphysical assessment of dualism and theism
   ; PROGRAMMER: Paul Thagard
   ; CREATED: 10-16-98
   ; UPDATED: 8-10-99

   ; *************************************************

    

    

   (defun real ()

   (setq *problem* 'reality)
   (clear-net)

    

    

   ; Materialist hypotheses:
   (proposition 'MH1 "Everything consists of matter and energy.")
   (proposition 'MH2 "Minds consist of matter and energy.")
   (proposition 'MH3 "The universe has always existed, or came to be
   sporadically.")
   (proposition 'MH4 "People are prone to fraud, illusion, and other
   psychological failings.")
   (proposition 'MH5 "People acquire beliefs and attitudes through education
   and socialization.")
   (proposition 'MH6 "Consciousness emerges from brain activity.")
   (proposition 'MH7 "Biological complexity emerges from natural selection.")
   (proposition 'MH8 "People are biological organisms.")
   (proposition 'MH9 "Brains near death undergo physical changes.")

   ; Dualist hypotheses:
   (proposition 'DH1 "Minds consist of matter and soul.")
   (proposition 'DH2 "Minds consist partly of soul.")
   (proposition 'DH3 "Minds consist partly of matter.")
   (proposition 'DH4 "People have free will.")
   (proposition 'DH5 "People survive after death.")
   (proposition 'DH6 "People have extra-sensory perception.")

   ; Theistic hypotheses:
   (proposition 'TH1 "God exists.")
   (proposition 'TH2 "God is all powerful.")
   (proposition 'TH3 "God created and designed the universe.")

   ; Evidence:
   (proposition 'E1 "Many physical phenomena.")
   (proposition 'E2 "Many chemical phenomena.")
   (proposition 'E3 "Many biological phenomena.")
   (proposition 'E4 "Vision correlates with brain activity.")
   (proposition 'E5 "Memory correlates with brain activity.")
   (proposition 'E6 "Memory correlates with brain activity.")
   (proposition 'E7 "People report near death experiences.")
   (proposition 'E8 "People report contact with the dead in seances.")
   (proposition 'E9 "People feel they have free will.")
   (proposition 'E10 "People have a moral sense.")
   (proposition 'E11 "Remote viewing experiments.")
   (proposition 'E12 "Telekinesis experiments.")
   (proposition 'E13 "Telepathy experiments.")
   (proposition 'E14 "People have consciousness.")
   (proposition 'E15 "The universe exists.")
   (proposition 'E16 "The universe has laws.")
   (proposition 'E17 "Biological complexity.")
   (proposition 'E18 "People report miracles.")
   (proposition 'E19 "People suffer from disease and natural disasters.")
   (proposition 'E20 "People suffer from the evil actions of others.")

    

    

   ; Contradictions:

   (contradict 'MH1 'DH1)
   (contradict 'MH1 'TH1)

    

   ; Materialist explanations:
   (imply '(MH1) 'MH2)
   (imply '(MH1) 'MH8)
   (explain '(MH1) 'E1)
   (explain '(MH1) 'E2)
   (explain '(MH1) 'E3)
   (explain '(MH2) 'E4)
   (explain '(MH2) 'E5)
   (explain '(MH2) 'E6)
   (explain '(MH2 MH5 MH9) 'E7)
   (explain '(MH2 MH4) 'E8)
   (explain '(MH2 MH5) 'E9)
   (explain '(MH2 MH5) 'E10)
   (explain '(MH2 MH4) 'E11)
   (explain '(MH2 MH4) 'E12)
   (explain '(MH2 MH4) 'E13)
   (explain '(MH2 MH6) 'E14)
   (explain '(MH2 MH3) 'E15) ; note E16 not explained
   (explain '(MH1 MH7) 'E17)
   (explain '(MH2 MH4) 'E18)
   (explain '(MH1 MH8) 'E19)
   (explain '(MH2 MH4) 'E20)

    

   ; Dualist explanations:
   (imply '(DH1) 'DH2)
   (imply '(DH1) 'DH3)
   (explain '(DH2) 'DH4)
   (explain '(DH2) 'DH5)
   (explain '(DH2) 'DH6)
   (explain '(DH3) 'E4)
   (explain '(DH3) 'E5)
   (explain '(DH3) 'E6)
   (explain '(DH5) 'E7)
   (explain '(DH5) 'E8)
   (explain '(DH4) 'E9)
   (explain '(DH2) 'E10)
   (explain '(DH6) 'E11)
   (explain '(DH6) 'E12)
   (explain '(DH6) 'E13)
   (explain '(DH2) 'E14)

   ; Theist explanations:
   (explain '(TH1) 'DH1) ; connection between theism and dualism
   (explain '(TH1 TH2) 'TH3)
   ; (explain '(TH3) 'E1) ; omitted for lack of detailed explanation
   ; (explain '(TH3) 'E2)
   ; (explain '(TH3) 'E3)
   (explain '(TH3) 'E15)
   (explain '(TH3) 'E16)
   (explain '(TH3) 'E17)
   (explain '(TH3) 'E18)
   (explain '(TH3 DH4) 'E20) ; E19?

    

   (data '(E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 E11 E12 E13 E14 E15 E16 E17 E18 E19
   E20))

   (make-competition) ; ECHO.2
   (reset-act (mysort *all-explainers*))
   (eval-cohere)
   )

    

    

   (defun imply (lst prop) (explain lst prop))

   (defun mysort (lst) (sort lst #'string-lessp))

    

    

    

    
