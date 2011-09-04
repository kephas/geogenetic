(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defclass geometrical-constraints-system ()
  ((universe :initarg :universe :reader gcs-universe)
   (entities :initform (make-hash-table))
   (unknowns :initform (make-hash-table))
   (parameters :initform (make-hash-table))
   (parameters-sequence :initarg :params :reader gcs-param-sequence)))

(define-hash-table-reader geometrical-constraints-system get-entity entities)
(define-hash-table-writer geometrical-constraints-system store-entity entities)

(define-hash-table-reader geometrical-constraints-system get-unknown unknowns)
(define-hash-table-writer geometrical-constraints-system store-unknown unknowns)

(define-hash-table-reader geometrical-constraints-system get-parameter parameters)
(define-hash-table-writer geometrical-constraints-system store-parameter parameters)

(defgeneric gcs-unknowns-sequence (system))
(defmethod gcs-unknowns-sequence (system)
  (sort (hash-keys (slot-value system 'unknowns)) #'string< :key #'symbol-name))


(defun resync-from-entities (system)
  "Redispatch entities to the UNKNOWNS and PARAMETERS hash tables, in
  case they have been replaced in the ENTITIES hash table."
  (dolist (slot '(unknowns parameters))
    (maphash (lambda (key value)
	       (declare (ignore value))
	       (setf (gethash key (slot-value system slot)) (gethash key (slot-value system 'entities))))
	     (slot-value system slot))))


(defmethod shared-clone :after ((object geometrical-constraints-system) (clone geometrical-constraints-system))
  (setf (slot-value clone 'universe) (slot-value object 'universe)
	(slot-value clone 'parameters-sequence) (slot-value object 'parameters-sequence))
  (maphash (lambda (key value)
	     (setf (gethash key (slot-value clone 'entities))
		   (clone value)))
	   (slot-value object 'entities))
  (dolist (slot '(unknowns parameters))
    (maphash (lambda (key value)
	       (declare (ignore value))
	       (setf (gethash key (slot-value clone slot)) (gethash key (slot-value clone 'entities))))
	     (slot-value object slot))))


(defmethod get-predicate-criterion ((gcs geometrical-constraints-system) predicate)
  (get-predicate-criterion (gcs-universe gcs) predicate))

(defmethod get-predicate-signature ((gcs geometrical-constraints-system) predicate)
  (get-predicate-signature (gcs-universe gcs) predicate))

(defmethod has-entity-p ((system geometrical-constraints-system) entity)
  (gethash entity (slot-value system 'entities)))

(defmethod add-entity ((system geometrical-constraints-system) entity type unknown)
  (let ((obj (make-instance 'geometrical-entity :type type)))
    (store-entity system entity obj)
    (if unknown
	(store-unknown system entity obj)
	(store-parameter system entity obj))))


(defclass sequential-valuation ()
  ((values :initarg :values :reader valuation-sequence)))

(defclass named-valuation ()
  ((values :initform (make-hash-table))))

(define-hash-table-reader named-valuation get-named-value values)
(define-hash-table-writer named-valuation store-named-value values)


(defun apply-valuation (system slot valuation)
  (maphash (lambda (name entity)
	     (upgrade-with-value (gcs-universe system) entity (get-named-value valuation name)))
	   (slot-value system slot)))

(defun add-value-names (names valuation)
  "Transform a sequantial valuation to a named one."
  (let ((named-vals (make-instance 'named-valuation)))
    (map nil (lambda (name value)
	       (store-named-value named-vals name value))
	 names
	 (valuation-sequence valuation))
    named-vals))


(defgeneric valuated-system (system valuation)
  (:documentation "Return a copy of the system where parameters are valuated"))

(defmethod valuated-system (system (valuation sequential-valuation))
  (valuated-system system (add-value-names (gcs-param-sequence system) valuation)))

(defmethod valuated-system (system (valuation named-valuation))
  (let ((copy (clone system)))
    (apply-valuation copy 'parameters valuation)
    copy))
