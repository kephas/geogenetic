(define (evolve population select crossover mutate)
  (map mutate (apply append (map crossover (select population)))))
