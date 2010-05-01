(defun add-entities-from-specifications (system specifications unknown)
  (named-let loop-specs ((specifications specifications))
    (if specifications
	(let* ((specification (first specifications))
	       (type (canonical-type-name system (first specification))))
	  (named-let loop-entities ((entities (rest specification)))
	    (if entities
		(progn
		  (add-entity system (first entities) type unknown)
		  (loop-entities (rest entities)))
		(loop-specs (rest specifications))))))))

(defun ensure-entity (system entity type)
  (if (has-entity-p system entity)
      (unless (eq type (entity-type (get-entity system entity)))
	(error 'gcs-type-error))
      (add-entity system entity type nil)))
