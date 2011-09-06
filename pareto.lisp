(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun pareto-fronts (population criteria)
  (labels ((better-on-all-criteria? (comparee compared-to)
	     (if (eq comparee compared-to)
		 nil
		 (let ((args (list comparee compared-to)))
		   (every (lambda (criterion) (apply criterion args)) criteria)))))
    (named-let make-fronts ((unscored-individuals population)
			    (remaining-population population)
			    (inidividuals-for-next-fronts '())
			    (current-front '())
			    (previous-fronts '()))
      (if (null unscored-individuals)
	  (if (null inidividuals-for-next-fronts)
	      (if (null current-front) ; should only be true when entire population is empty...
		  previous-fronts
		  (cons current-front previous-fronts))
	      (make-fronts inidividuals-for-next-fronts
			   inidividuals-for-next-fronts
			   '()
			   '()
			   (cons current-front previous-fronts)))
	  (let ((current-individual (first unscored-individuals)))
	    (if (named-let in-pareto-front? ((individuals remaining-population))
		  (if (null individuals)
		      t
		      (if (better-on-all-criteria? (first individuals) current-individual)
			  nil
			  (in-pareto-front? (rest individuals)))))
		(make-fronts (rest unscored-individuals)
			     remaining-population
			     inidividuals-for-next-fronts
			     (cons current-individual current-front)
			     previous-fronts)
		(make-fronts (rest unscored-individuals)
			     remaining-population
			     (cons current-individual inidividuals-for-next-fronts)
			     current-front
			     previous-fronts)))))))


(defun make-affine-score-source (start &optional (step 1))
  "Return a function that returns START and then numbers in-/decreasing by STEP."
  (let ((start (- start step)))
    (lambda () (incf start step))))

(defun make-deducting-score-source (items &key (end 0) (step 1))
  "Return a function that returns numbers decreasing by STEP associated with each element of ITEMS
such as the last is associated with END."
  (make-affine-score-source (+ end (* step (length items))) (- step)))

; pretty slick: two nested mutually recursive local functions...
(defun make-pareto-score (fronts &optional (score-source (make-affine-score-source 1)))
  (let ((scores (named-let score-fronts ((fronts fronts)
					 (current-score (funcall score-source))
					 (scores '()))
                  (if (null fronts)
                      scores
                      (named-let score-individuals ((individuals (first fronts))
						    (scores scores))
			(if (null individuals)
                            (score-fronts (rest fronts) (funcall score-source) scores)
                            (score-individuals (rest individuals)
                                               (cons (cons (first individuals) current-score) scores))))))))
    (lambda (individual)
      (cdr (assoc individual scores)))))
