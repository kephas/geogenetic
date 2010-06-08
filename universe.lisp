(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defclass geometrical-universe ()
  ((predicate-criteria :initform (make-hash-table))
   (predicate-signatures :initform (make-hash-table))))

(define-hash-table-reader geometrical-universe get-predicate-criterion predicate-criteria)
(define-hash-table-writer geometrical-universe store-predicate-criterion predicate-criteria)

(define-hash-table-reader geometrical-universe get-predicate-signature predicate-signatures)
(define-hash-table-writer geometrical-universe store-predicate-signature predicate-signatures)
