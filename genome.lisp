(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defgeneric entity->phenotype (universe entity))

(defmethod entity->phenotype ((universe (eql *euclidian*)) (entity geometrical-entity-with-value))
  (list (entity-value entity)))

(defmethod entity->phenotype ((universe (eql *euclidian*)) (entity geometrical-entity-with-coordinates))
  (clone (entity-coordinates entity)))

(defun gcs->phenotype (system)
  (mapcan (lambda (entity) (entity->phenotype (gcs-universe system) entity))
	  (mapcar (lambda (name)
		    (get-entity system name))
		  (gcs-unknowns-sequence system))))
