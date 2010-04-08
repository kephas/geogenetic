(define (pareto-fronts population criteria)
  (define (better-on-all-criteria? comparee compared-to)
    (if (eq? comparee compared-to)
        #f
        (let rec ((remaining-criteria criteria))
          (if (null? remaining-criteria)
              #t
              (if ((first remaining-criteria) comparee compared-to)
                  (rec (rest remaining-criteria))
                  #f)))))
  (let make-fronts ((unscored-individuals population)
                    (inidividuals-for-next-fronts '())
                    (current-front '())
                    (previous-fronts '()))
    (if (null? unscored-individuals)
        (if (null? inidividuals-for-next-fronts)
            (if (null? current-front) ; should only be true when entire population is empty...
                previous-fronts
                (cons current-front previous-fronts))
            (make-fronts inidividuals-for-next-fronts '() '() (cons current-front previous-fronts)))
        (let ((current-individual (first unscored-individuals)))
          (if (let in-pareto-front? ((individuals population))
                (if (null? individuals)
                    #t
                    (if (better-on-all-criteria? (first individuals) current-individual)
                        #f
                        (in-pareto-front? (rest individuals)))))
              (make-fronts (rest unscored-individuals)
                           inidividuals-for-next-fronts
                           (cons current-individual current-front)
                           previous-fronts)
              (make-fronts (rest unscored-individuals)
                           (cons current-individual inidividuals-for-next-fronts)
                           current-front
                           previous-fronts))))))
