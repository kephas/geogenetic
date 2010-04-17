(define (test name value)
  (display name)
  (display ": ")
  (display (if (boolean? value)
               (if value "GOOD" "BAD")
               value))
  (display "\n"))


(load "pareto.scm")

(define (make-criteria op)
  (define (make-criterion index)
    (lambda (comparee compared-to)
      (op (list-ref comparee index) (list-ref compared-to index))))
  (let loop ((index 2))
    (cons (make-criterion index) (if (= 0 index) '() (loop (- index 1))))))

(define triples '((8 9 9)(1 2 3)(9 8 9)(4 5 6)(2 1 3)(5 4 7)))

(test "pareto"
      (equal?
       (map (make-pareto-score (pareto-fronts triples (make-criteria >)) 1) triples)
       '(3 1 3 2 1 2)))

(load "crossover.scm")

(define (int-range start count)
  (let loop ((number start)
             (remaining count)
             (acc '()))
    (if (zero? remaining)
        (reverse acc)
        (loop (add1 number) (sub1 remaining) (cons number acc)))))

(define alice (int-range 10 10))
(define bob (int-range 20 10))

(test "1p-crossover"
      ((make-1p-crossover-operator) alice bob))

(load "mutation.scm")

(define carol (int-range 1 20))

(test "mutation"
      ((make-probabilistic-mutation-operator 25 (lambda (n) (+ n 100))) carol))
