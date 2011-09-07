(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun submapcar (function list)
  (mapcar (lambda (sublist) (mapcar function sublist)) list))

(defun evolve (population select crossover mutate individual->genes genes->individual)
  (mapcar genes->individual (mapcar mutate (mapcan crossover (submapcar individual->genes (funcall select population))))))
