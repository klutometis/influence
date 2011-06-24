#!/usr/bin/env chicken-scheme
(import scheme chicken)
(use influence)

(with-problem
 (make-problem)
 (lambda ()
   (propose! 'oj-abusive)
   (propose! 'oj-beat-nicole)
   (propose! 'oj-killed-nicole)
   (propose! 'nicole-and-ron-were-killed)
   (propose! 'blood-on-sock)
   (propose! 'blood-in-ojs-car)
   (propose! 'bloody-glove)
   (propose! 'bloody-gate)
   (propose! 'fuhrman-lied)
   (propose! 'edta-on-sock)
   (propose! 'drug-dealers-killed-nicole)
   (propose! 'the-lapd-framed-oj)

   (explain! '(oj-beat-nicole oj-killed-nicole) 'oj-abusive)
   (explain! '(blood-on-sock
               blood-in-ojs-car
               bloody-glove
               bloody-gate
               fuhrman-lied
               edta-on-sock)
             'the-lapd-framed-oj)
   (explain! '(nicole-and-ron-were-killed)
             'drug-dealers-killed-nicole)

   (contradict! '(drug-dealers-killed-nicole
                  the-lapd-framed-oj)
                'oj-killed-nicole)

   (evidence! '(oj-beat-nicole
                nicole-and-ron-were-killed
                blood-on-sock
                blood-in-ojs-car
                bloody-glove
                bloody-gate
                fuhrman-lied
                edta-on-sock)) 

   (display-problem/dot
    (solve! (connectionist
             (make-animation-processor "oj.gif" "OJ")
             (make-time-series-processor "oj-series.png"))))))
