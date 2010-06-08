(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defmacro with-functions (names &body body)
  `(labels (,@(mapcar (lambda (name-spec)
                        (if (consp name-spec)
                            `(,(car name-spec) (&rest rest) (apply ,(cadr name-spec) rest))
                            `(,name-spec (&rest rest) (apply ,name-spec rest))))
                      names))
     ,@body))

(defmacro named-let (name binds &body body)
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))


(defmacro define-hash-table-reader (class name slot)
  `(defmethod ,name ((object ,class) key)
     (gethash key (slot-value object ',slot))))

(defmacro define-hash-table-writer (class name slot)
  `(defmethod ,name ((object ,class) key value)
     (setf (gethash key (slot-value object ',slot)) value)))
