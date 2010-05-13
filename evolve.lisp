(defun evolve (population select crossover mutate)
  (mapcar mutate (mapcan crossover (funcall select population))))
