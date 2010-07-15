(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defmethod print-object ((object geometrical-constraints-system) stream)
  (format stream "#<GEOMETRICAL-CONSTRAINTS-SYSTEM ~a>"
	  (let ((keys))
	    (maphash (lambda (key value)
		       (declare (ignore value))
		       (push key keys))
		     (slot-value object 'entities))
	    (reverse keys))))

(defmethod print-object ((object geometrical-entity) stream)
  (format stream "#<GEOMETRICAL ~a .>" (entity-type object)))

(defmethod print-object ((object geometrical-entity-with-value) stream)
  (format stream "#<GEOMETRICAL ~a = ~a>" (entity-type object) (entity-value object)))

(defmethod print-object ((object geometrical-entity-with-coordinates) stream)
  (format stream "#<GEOMETRICAL ~a : ~a>" (entity-type object) (entity-coordinates object)))
