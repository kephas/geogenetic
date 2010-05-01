(defclass geometrical-entity ()
  ((type :initarg :type)))

(defmethod entity-type ((entity geometrical-entity))
  (slot-value entity 'type))
