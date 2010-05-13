(defclass geometrical-constraints-system ()
  ((entities :initform (make-hash-table))
   (unknowns :initform (make-hash-table))
   (parameters :initform (make-hash-table))
   (predicate-criteria :initform (make-hash-table))
   (predicate-signatures :initform (make-hash-table))))


(defmacro define-hash-table-reader (class name slot)
  `(defmethod ,name ((object ,class) key)
     (gethash key (slot-value object ',slot))))

(defmacro define-hash-table-writer (class name slot)
  `(defmethod ,name ((object ,class) key value)
     (setf (gethash key (slot-value object ',slot)) value)))

(define-hash-table-reader geometrical-constraints-system get-entity entities)
(define-hash-table-writer geometrical-constraints-system store-entity entities)

(define-hash-table-reader geometrical-constraints-system get-unknown unknowns)
(define-hash-table-writer geometrical-constraints-system store-unknown unknowns)

(define-hash-table-reader geometrical-constraints-system get-parameter parameters)
(define-hash-table-writer geometrical-constraints-system store-parameter parameters)

(define-hash-table-reader geometrical-constraints-system get-predicate-criterion predicate-criteria)
(define-hash-table-writer geometrical-constraints-system store-predicate-criterion predicate-criteria)

(define-hash-table-reader geometrical-constraints-system get-predicate-signature predicate-signatures)
(define-hash-table-writer geometrical-constraints-system store-predicate-signature predicate-signatures)


(defmethod has-entity-p ((system geometrical-constraints-system) entity)
  (gethash entity (slot-value system 'entities)))

(defmethod add-entity ((system geometrical-constraints-system) entity type unknown)
  (let ((obj (make-instance 'geometrical-entity :type type)))
    (store-entity system entity obj)
    (if unknown
	(store-unknown system entity obj)
	(store-parameter system entity obj))))
