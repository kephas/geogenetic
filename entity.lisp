(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defclass geometrical-entity ()
  ((type :initarg :type :reader entity-type)))

(defclass geometrical-entity-with-coordinates (geometrical-entity)
  ((coordinates :initarg :coordinates :reader entity-coordinates)))
