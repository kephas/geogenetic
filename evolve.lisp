(defun evolve (population select crossover mutate)
  (mapcar mutate (apply #'append (mapcar crossover (funcall select population)))))