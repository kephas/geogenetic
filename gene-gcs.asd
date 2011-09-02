(defpackage :thierry-technologies.com/2010/06/gene-gcs-system
  (:use :common-lisp :asdf))

(in-package :thierry-technologies.com/2010/06/gene-gcs-system)

(defsystem "gene-gcs"
  :description "Geometrical Constraints System solver using genetic algorithms"
  :version "0.1.0"
  :author "Pierre Thierry <pierre.thierry@thierry-technologies.com>"
  :licence "GPL"
  :depends-on ("cl-utilities" "cl-clone")
  :components ((:file "package")
               (:file "macros")
	       (:file "misc")
	       (:file "universe")
	       (:file "gcs")
	       (:file "entity")
	       (:file "constraint")
	       (:file "definitions")
	       (:file "sysdef")
	       (:file "pareto")
	       (:file "select")
	       (:file "crossover")
	       (:file "mutation")
	       (:file "evolve")
	       (:file "debug"))

  :serial t)
