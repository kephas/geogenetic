(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun error-margin (val1 val2)
  "Calculates the difference between two values"
  (let ((val1 (entity-value val1))
	(val2 (entity-value val2)))
    (abs (- val1 val2))))

(defun distance-between (point1 point2)
  (sqrt (reduce #'+ (mapcar (lambda (coord1 coord2)
			      (expt (abs (- coord1 coord2)) 2))
			    (entity-coordinates point1)
			    (entity-coordinates point2)))))

(defun magnitude (vector)
  (sqrt (reduce #'+ (mapcar (lambda (x) (expt x 2)) (entity-coordinates vector)))))

(defun scalar-product (vector1 vector2)
  (reduce #'+ (mapcar #'* (entity-coordinates vector1) (entity-coordinates vector2))))

(defun vectors-angle (vector1 vector2)
  (/ (scalar-product vector1 vector2) (* (magnitude vector1) (magnitude vector2))))

(defun lines-angle (line1 line2)
  (vectors-angle (line-vector line1) (line-vector line2)))

(defun 2points->vector (point1 point2)
  (make-instance 'geometrical-entity-with-coordinates
		 :type 'vector
		 :coordinates (mapcar #'- (entity-coordinates point2) (entity-coordinates point1))))


(defun hash-keys (hash-table)
  (let ((keys))
    (maphash (lambda (key value)
	       (declare (ignore value))
	       (push key keys))
	     hash-table)
    keys))
