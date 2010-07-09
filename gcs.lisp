(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defclass geometrical-constraints-system ()
  ((universe :initarg :universe :reader gcs-universe)
   (entities :initform (make-hash-table))
   (unknowns :initform (make-hash-table))
   (parameters :initform (make-hash-table))))

(define-hash-table-reader geometrical-constraints-system get-entity entities)
(define-hash-table-writer geometrical-constraints-system store-entity entities)

(define-hash-table-reader geometrical-constraints-system get-unknown unknowns)
(define-hash-table-writer geometrical-constraints-system store-unknown unknowns)

(define-hash-table-reader geometrical-constraints-system get-parameter parameters)
(define-hash-table-writer geometrical-constraints-system store-parameter parameters)


(defmethod get-predicate-criterion ((gcs geometrical-constraints-system) predicate)
  (get-predicate-criterion (gcs-universe gcs) predicate))

(defmethod predicate-signature ((gcs geometrical-constraints-system) predicate)
  (get-predicate-signature (gcs-universe gcs) predicate))

(defmethod has-entity-p ((system geometrical-constraints-system) entity)
  (gethash entity (slot-value system 'entities)))

(defmethod add-entity ((system geometrical-constraints-system) entity type unknown)
  (let ((obj (make-instance 'geometrical-entity :type type)))
    (store-entity system entity obj)
    (if unknown
	(store-unknown system entity obj)
	(store-parameter system entity obj))))

; no alias system for now
(defmethod canonical-type-name (system name)
  name)
