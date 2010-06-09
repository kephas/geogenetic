(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defvar *euclidian* (make-instance 'geometrical-universe))

(defconstraint *euclidian* distpp ((d distance)(p1 point)(p2 point)) <
  (error-margin (entity d) (distance-between (entity p1) (entity p2))))
