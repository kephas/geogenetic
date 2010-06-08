(defmacro defconstraint (universe name args predicate sys-var &body body)
  `(progn
     (store-predicate-signature ,universe ,name ',(mapcar #'second args))
     (store-predicate-criteria ,universe ,name
			       (lambda (entities)
				 (destructuring-bind ,(mapcar #'first args) entities
				   (lambda (sys1 sys2)
				     (labels ((score (,sys-var)
						,@body))
				       (funcall ,predicate (score sys1) (score sys2)))))))))
