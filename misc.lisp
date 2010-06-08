(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun error-margin (val1 val2)
  (abs (- val1 val2)))

(defun distance-between (point1 point2)
  (sqrt (reduce #'+ (mapcar (lambda (coord1 coord2)
			      (expt (abs (- coord1 coord2)) 2))
			    (point-coordinates point1)
			    (point-coordinates point2)))))
