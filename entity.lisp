(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defclass geometrical-entity ()
  ((type :initarg :type :reader entity-type)))

(defclass geometrical-entity-with-coordinates (geometrical-entity)
  ((coordinates :initarg :coordinates :reader entity-coordinates)))

(defclass geometrical-entity-with-value (geometrical-entity)
  ((value :initarg :value :reader entity-value)))

; any object that is not a geometrical entity with value is
; its own value
(defmethod entity-value (obj)
  obj)



(defgeneric %entity-type-class (universe type)
  (:documentation "Return the class that is able to represent TYPE"))

(defun upgrade-entity-for-type (universe obj)
  (change-class obj (%entity-type-class universe (entity-type obj))))


(defgeneric fill-entity-with-value (obj value))

(defun upgrade-with-value (universe entity value)
  "Change ENTITY, in UNIVERSE, to what is described by VALUE."
  (upgrade-entity-for-type universe entity)
  (fill-entity-with-value entity value))



(defmethod fill-entity-with-value ((obj geometrical-entity-with-coordinates) (value list))
  (setf (slot-value obj 'coordinates) value))

(defmethod fill-entity-with-value ((obj geometrical-entity-with-value) (value number))
  (setf (slot-value obj 'value) value))


(defmethod shared-clone :after ((object geometrical-entity) (clone geometrical-entity))
  (setf (slot-value clone 'type) (entity-type object)))

(defmethod shared-clone :after ((object geometrical-entity-with-coordinates) (clone geometrical-entity-with-coordinates))
  (setf (slot-value clone 'coordinates) (entity-coordinates object)))

(defmethod shared-clone :after ((object geometrical-entity-with-value) (clone geometrical-entity-with-value))
  (setf (slot-value clone 'value) (entity-value object)))
