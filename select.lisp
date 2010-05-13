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

(defun stochastic-universal-sampling (population score precision)
  (let* ((count (length population))
         (max (apply + (mapcar score population)))
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
