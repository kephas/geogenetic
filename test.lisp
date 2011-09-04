(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defvar %sys)
(defvar %constraints)

(defun read-test (file)
  (let ((data (cdar (read-definitions-files file *euclidian-base*))))
    (setf %sys (first data)
	  %constraints (second data))))
