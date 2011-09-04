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



(defgeneric %phenotype->entity (universe entity phenotype))

(defgeneric phenotype-size (universe entity))

(defun phenotype->entity (universe type phenotype)
  (let ((entity (make-instance (%entity-type-class universe type) :type type)))
    (assert-phenotype-size (phenotype-size universe entity) phenotype)
    (%phenotype->entity universe entity phenotype)))


(defun assert-phenotype-size (size phenotype)
  (when (< (length phenotype) size)
    (error "Ran out of phenotype…")))

(defmethod phenotype-size ((universe euclidian-universe) (entity symbol))
  (phenotype-size universe (instantiate-type universe entity)))

(defmethod phenotype-size ((universe euclidian-universe) (entity geometrical-entity-with-value))
  1)

(defmethod phenotype-size ((universe euclidian-universe) (entity geometrical-entity-with-coordinates))
  (universe-dimensions universe))


(defmethod %phenotype->entity ((universe euclidian-universe) (entity geometrical-entity-with-value) phenotype)
  (let ((remaining (subseq phenotype 1)))
    (setf (slot-value entity 'value) (nth 0 phenotype))
    (values entity remaining)))

(defmethod %phenotype->entity ((universe euclidian-universe) (entity geometrical-entity-with-coordinates) phenotype)
  (let ((dims (universe-dimensions universe)))
    (let ((remaining (subseq phenotype dims)))
      (setf (slot-value entity 'coordinates) (subseq phenotype 0 dims))
      (values entity remaining))))


(defun typed-names (system names)
  (mapcar (lambda (name)
	    (list (entity-type (get-entity system name)) name))
	  names))

(defun phenotype->gcs (universe reference-system phenotype)
  (let ((system (clone reference-system)))
    (named-let rec ((phenotype phenotype)
		    (types (typed-names system (gcs-unknowns-sequence system))))
      (if types
	  (let ((type (first (first types)))
		(name (second (first types))))
	    (multiple-value-bind (entity remaining-phenotype)
		(phenotype->entity universe type phenotype)
	      (store-entity system name entity)
	      (rec remaining-phenotype (rest types))))
	  (progn
	    (when phenotype
	      (warn "There's still a bit of phenotype left. Doggy bag? ~a" phenotype))
	    (resync-from-entities system)
	    system)))))



(defun gcs-phenotype-length (system)
  (reduce #'+ (mapcar (lambda (type)
			(phenotype-size (gcs-universe system) type))
		      (mapcar #'first (typed-names system (gcs-unknowns-sequence system))))))

(defun genotype->phenotype (size system genotype)
  (named-let rec ((genotype genotype)
		  (counter (gcs-phenotype-length system))
		  (phenotype nil))
    (if (zerop counter)
	phenotype
	(multiple-value-bind (value remaining-genotype)
	    (extract-value size genotype)
	  (rec remaining-genotype (1- counter) (cons value phenotype))))))
