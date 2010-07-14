(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defgeneric parse-value (universe type value)
  (:documentation "Returns an entity of type TYPE that,
in UNIVERSE, is described by VALUE."))


(defvar *euclidian* (make-instance 'geometrical-universe))

(defconstraint *euclidian* distpp ((d distance)(p1 point)(p2 point)) <
  (error-margin (entity d) (distance-between (entity p1) (entity p2))))

(defconstraint *euclidian* anglell ((a angle)(l1 line) (l2 line)) <
  (error-margin (entity a) (lines-angle (entity l1) (entity p2))))

(defconstraint *euclidian* angleppp ((a angle) (p1 point) (p2 point) (p3 point)) <
  (error-margin a (vectors-angle (2points->vector p2 p1) (2points->vector p2 p3))))

(defmethod parse-value ((universe (eql *euclidian*)) (type (eql 'angle)) (value number))
  (make-instance 'geometrical-entity-with-value
		 :type 'angle
		 :value value))

(defmethod parse-value ((universe (eql *euclidian*)) (type (eql 'point)) (value list))
  (make-instance 'geometrical-entity-with-coordinates
		 :type 'point
		 :coordinates value))

(defmethod parse-value ((universe (eql *euclidian*)) (type (eql 'distance)) (value number))
  (make-instance 'geometrical-entity-with-value
		 :type 'distance
		 :value value))
