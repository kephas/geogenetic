(define (pareto-fronts population criteria)
  (define (better-on-all-criteria? comparee compared-to)
    (if (eq comparee compared-to)
        #f
        (let rec ((remaining-criteria criteria))
          (if (null? remaining-criteria)
              #t
              (if ((first remaining-criteria) comparee compared-to)
                  (rec (rest remaining-criteria))
                  #f)))))
  (let rec ((individuals population))
    (
