(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defclass geometrical-universe ()
  ((predicate-criteria :initform (make-hash-table))
   (predicate-signatures :initform (make-hash-table))
   (dimensions :initform nil)))

(define-hash-table-reader geometrical-universe get-predicate-criterion predicate-criteria)
(define-hash-table-writer geometrical-universe store-predicate-criterion predicate-criteria)

(define-hash-table-reader geometrical-universe get-predicate-signature predicate-signatures)
(define-hash-table-writer geometrical-universe store-predicate-signature predicate-signatures)

(defmethod shared-clone :after ((object geometrical-universe) (clone geometrical-universe))
  (setf (slot-value clone 'predicate-criteria) (slot-value object 'predicate-criteria)
	(slot-value clone 'predicate-signatures) (slot-value object 'predicate-signatures)
	(slot-value clone 'dimensions) (slot-value object 'dimensions)))
