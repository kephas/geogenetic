(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defgeneric entity->phenotype (universe entity))

(defmethod entity->phenotype ((universe euclidian-universe) (entity geometrical-entity-with-value))
  (list (entity-value entity)))

(defmethod entity->phenotype ((universe euclidian-universe) (entity geometrical-entity-with-coordinates))
  (clone (entity-coordinates entity)))

(defun gcs->phenotype (system)
  (mapcan (lambda (entity) (entity->phenotype (gcs-universe system) entity))
	  (mapcar (lambda (name)
		    (get-entity system name))
		  (gcs-unknowns-sequence system))))


(defun typed-names (system names)
  (mapcar (lambda (name)
	    (list (entity-type (get-entity system name)) name))
	  names))


(defgeneric %phenotype->entity (universe entity phenotype))

(defun phenotype->entity (universe type phenotype)
  (%phenotype->entity universe (%entity-type-class universe type) phenotype))


(defmethod %phenotype->entity ((universe euclidian-universe) (entity geometrical-entity-with-value) phenotype)
  (let ((remaining (subseq phenotype 1)))
    (setf (slot-value entity 'value) (nth 0 phenotype))
    (values entity remaining)))

(defmethod %phenotype->entity ((universe euclidian-universe) (entity geometrical-entity-with-coordinates) phenotype)
  (let* ((dims (universe-dimensions universe))
	 (remaining (subseq phenotype dims)))
    (setf (slot-value entity 'coordinates) (subseq phenotype 0 dims))
    (values entity remaining)))
