(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defclass geometrical-entity ()
  ((type :initarg :type)))

(defmethod entity-type ((entity geometrical-entity))
  (slot-value entity 'type))
