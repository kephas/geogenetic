(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defmacro defconstraint (universe name args predicate &body body)
  "Defines a named constraint within a geometrical universe.
ARGS is a list of the form ((name1 type1) (name2 type2) ... (nameN typeN))"
  (with-unique-names (sys-var)
    `(progn
       (store-predicate-signature ,universe ',name ',(mapcar #'second args))
       (store-predicate-criterion ,universe ',name
				  (lambda (entities)
				    (destructuring-bind ,(mapcar #'first args) entities
				      (lambda (sys1 sys2)
					(labels ((score (,sys-var)
						   (labels ((entity (name)
							      (get-entity ,sys-var name)))
						     ,@body)))
					  (funcall ,predicate (score sys1) (score sys2))))))))))
