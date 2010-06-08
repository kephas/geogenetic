(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun make-probabilistic-mutation-operator (probability flip)
  (lambda (individual)
    (mapcar (lambda (bit)
	      (if (< (random 100) probability)
		  (funcall flip bit)
		  bit))
	    individual)))
