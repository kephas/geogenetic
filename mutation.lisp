(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun make-probabilistic-mutation-operator (genome-size probability)
  (labels ((%not (bit) (boole boole-xor bit 1)))
    (lambda (genome)
      (named-let rec ((counter genome-size)
		      (genome genome))
	(if (< counter 0)
	    genome
	    (if (< (random 1.0) probability)
		(let ((place (byte 1 counter)))
		  (rec (1- counter) (dpb (%not (ldb place genome)) place genome)))
		(rec (1- counter) genome)))))))
