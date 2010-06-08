(in-package :thierry-technologies.com/2010/06/gene-gcs)

(defun evolve (population select crossover mutate)
  (mapcar mutate (mapcan crossover (funcall select population))))
