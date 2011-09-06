(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun average (numbers)
  (if (null numbers)
      0
      (named-let rec ((numbers numbers)
		       (sum 0)
		       (count 0))
        (if (null numbers)
            (/ sum count)
            (rec (rest numbers) (+ sum (first numbers)) (1+ count))))))

(defun average-score (population score)
  (average (mapcar score population)))

(defun stochastic-universal-sampling (population score &optional (precision (length population)))
  (let* ((count (length population))
         (max (reduce #'+ (mapcar score population)))
         (interval (/ max count))
         (start (/ (* (random precision) interval) precision)))
    (named-let sample ((population population)
		       (count count)
		       (pos start)
		       (sum-of-previous-scores 0)
		       (sampling '()))
      (if (zerop count)
          sampling
          (let ((adjusted-score (+ sum-of-previous-scores (funcall score (first population)))))
            (if (<= pos adjusted-score)
                (sample population (1- count) (+ pos interval) sum-of-previous-scores (cons (first population) sampling))
                (sample (rest population) count pos adjusted-score sampling)))))))


(defun make-random-partial-order (&optional (size 1.0))
  (let ((ranks (make-hash-table)))
    (lambda (x y)
      (dolist (item (list x y))
	(unless (gethash item ranks)
	  (setf (gethash item ranks) (random size))))
      (< (gethash x ranks) (gethash y ranks)))))

(defun shuffle (population)
  (sort population (make-random-partial-order (length population)) :key (lambda (x) (list x))))


(defun mate (population)
  (named-let rec ((population population)
		  (couples))
    (if population
	(rec (rest (rest population)) (cons (list (first population) (second population)) couples))
	couples)))


(defun make-stochastic-pareto-selection (criteria)
  (lambda (population)
    (mate (shuffle (stochastic-universal-sampling population (make-pareto-score (pareto-fronts population criteria)))
