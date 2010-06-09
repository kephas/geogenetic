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
