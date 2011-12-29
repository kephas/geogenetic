(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun hash-keys (hash-table)
  (let ((keys))
    (maphash (lambda (key value)
	       (declare (ignore value))
	       (push key keys))
	     hash-table)
    keys))
