(load "pareto.scm")

(define (make-criteria op)
  (define (make-criterion index)
    (lambda (comparee compared-to)
      (op (list-ref comparee index) (list-ref compared-to index))))
  (let loop ((index 2))
    (cons (make-criterion index) (if (= 0 index) '() (loop (- index 1))))))

(define triples '((1 2 3)(2 1 3)(4 5 6)(5 4 7)(9 8 9)(8 9 9)))

(display (pareto-fronts triples (make-criteria >)))