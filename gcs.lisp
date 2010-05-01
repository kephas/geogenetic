(defclass geometrical-constraints-system ()
  ((entities :initform (make-hash-table))
   (unknowns :initform (make-hash-table))
   (parameters :initform (make-hash-table))
   (predicate-criteria :initform (make-hash-table))
   (predicate-signatures :initform (make-hash-table))))


(defmacro define-hash-table-reader (class name slot)
  `(defmethod ,name ((object ,class) key)
     (gethash key (slot-value object ',slot))))

(define-hash-table-reader geometrical-constraints-system get-entity entities)
(define-hash-table-reader geometrical-constraints-system get-predicate-criterion predicate-criteria)
(define-hash-table-reader geometrical-constraints-system get-predicate-signature predicate-signatures)


(defmethod has-entity-p ((system geometrical-constraints-system) entity)
  (gethash entity (slot-value system 'entities))
