   ; FILE: IMP.lisp
   ; PURPOSE: basic functions for IMP, program for modelling impression
   formation
   ; Created Paul Thagard, 10-94
   ; This is based directly on DECO, and replaces SACS.lisp.

    

    

   ; *********************************
   ; * FUNCTIONS FOR PARSING IMP INPUT
   ; *********************************

   ; OBSERVED notes that a person has a particular feature, which may be a
   stereotype
   ; trait, or behavior. Degree is an optional factor that indicates to what
   ; degree the person has the feature.

   (defun observed (person feature &optional (degree 1))
   (note-unit feature)
   (addfeature person feature)
   (make-sym-link 'special feature (* degree *excit-weight*))
   (print-si person " is observed as: " feature " with degree " degree)
   )

   ; ADDFEATURE lists the feature on the person atom.
   (defun addfeature (person feature)
   (setf (get person 'features)
   (pushnew feature (get person 'features))
   )
   )

   ; ASSOCIATE establishes the positive and negative links between
   ; positively and negatively associated features.
   ; Positive associations are multiples of *excit-weight*.
   ; Negative associations are multiples of *inhib-weight*

   (defun associate (feature1 feature2 &optional (degree 1))
   (let ((weight (if (> degree 0) (* *excit-weight* degree)
   (* *inhib-weight* degree -1) ; negative association
   )
   )
   )
   (print-si feature1 " is associated with " feature2 " to degree " degree)
   (make-sym-link feature1 feature2 weight)
   (note-unit feature1) (note-unit feature2)
   )
   )

   ; IMP-RUN runs an experimental condition. The person should be the same
   ; as that indicated in the OBSERVED input.

   (defun imp-run (person)
   (my-print "Forming impression of " person)
   ;
   ; (graph *all-units*)
   (run-exp)
   (store-results person)
   )

   ; STORE-RESULTS notes the final activation values for all
   ; potential features of the person.

   (defun store-results (person)
   (dolist (unit *all-units*)
   (setf (get person unit)
   (activation unit)
   )
   )
   )

   ; COMPARE provides an easy way to compare the results across conditions.
   ; Each condition is associated with a different name, and the
   ; important features are the ones to be compared.

   (defun compare (person1 person2 feature)
   (my-print person1 " is " feature " to degree " (get person1 feature))
   (my-print person2 " is " feature " to degree " (get person2 feature))
   (my-print "The difference is " (- (get person1 feature)
   (get person2 feature))
   )
   )
