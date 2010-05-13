(defun pareto-fronts (population criteria)
  (labels ((better-on-all-criteria? (comparee compared-to)
	     (if (eq comparee compared-to)
		 nil
		 (named-let rec ((remaining-criteria criteria))
		   (if (null remaining-criteria)
		       t
		       (if (funcall (first remaining-criteria) comparee compared-to)
			   (rec (rest remaining-criteria))
			   nil))))))
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

; pretty slick: two nested mutually recursive local functions...
(defun make-pareto-score (fronts starting-score)
  (let ((scores (named-let score-fronts ((fronts fronts)
					 (current-score starting-score)
					 (scores '()))
                  (if (null fronts)
                      scores
                      (named-let score-individuals ((individuals (first fronts))
						    (scores scores))
			(if (null individuals)
                            (score-fronts (rest fronts) (1+ current-score) scores)
                            (score-individuals (rest individuals)
                                               (cons (cons (first individuals) current-score) scores))))))))
    (lambda (individual)
      (cdr (assoc individual scores)))))
