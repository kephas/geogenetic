(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun make-point-based-crossover (genome-size cross-points-source)
  (lambda (genomes)
    (let ((genome1 (first genomes))
	  (genome2 (second genomes)))
      (named-let rec ((pos genome-size)
		      (points (funcall cross-points-source))
		      (child1 0)
		      (child2 0))
	(if points
	    (let* ((next (first points))
		   (bytespec (byte (- pos next) next)))
	      (rec next (rest points)
		   (dpb (ldb bytespec genome1) bytespec child2)
		   (dpb (ldb bytespec genome2) bytespec child1)))
	    (list child1 child2))))))

(defun make-npoints-source (genome-size count)
  (lambda ()
    (named-let rec ((ceiling (1+ genome-size))
		    (counter count)
		    (acc nil))
      (if (zerop counter)
	  (append (reverse acc) '(0))
	  (let ((point (random ceiling)))
	    (rec point (1- counter) (cons point acc)))))))

(defun make-probabilistic-point-source (genome-size probability)
  (lambda ()
    (named-let rec ((counter genome-size)
		    (acc nil))
      (if (zerop counter)
	  (append (reverse acc) '(0))
	  (if (< (random 1.0) probability)
	      (rec (1- counter) (cons counter acc))
	      (rec (1- counter) acc))))))
