(in-package :thierry-technologies.com/2010/06/gene-gcs)


#| Various geometrical functions |#

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



#| Euclidian geometry |#

(defclass euclidian-universe (geometrical-universe)())

(defvar *euclidian-base* (make-instance 'euclidian-universe))


(defconstraint *euclidian-base* distpp ((d distance)(p1 point)(p2 point)) #'<
  (error-margin (entity d) (distance-between (entity p1) (entity p2))))

(defconstraint *euclidian-base* anglell ((a angle)(l1 line)(l2 line)) #'<
  (error-margin (entity a) (lines-angle (entity l1) (entity p2))))

(defconstraint *euclidian-base* angleppp ((a angle) (p1 point) (p2 point) (p3 point)) #'<
  (error-margin a (vectors-angle (2points->vector p2 p1) (2points->vector p2 p3))))


(defmethod %entity-type-class ((universe euclidian-universe) (type (eql 'angle)))
  'geometrical-entity-with-value)

(defmethod %entity-type-class ((universe euclidian-universe) (type (eql 'distance)))
  'geometrical-entity-with-value)

(defmethod %entity-type-class ((universe euclidian-universe) (type (eql 'point)))
  'geometrical-entity-with-coordinates)
