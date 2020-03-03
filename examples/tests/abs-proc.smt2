;! Cuvee -z3

(set-logic ALL)

(define-proc abs-proc ((x Int)) ((y Int))
    (if (< x 0) (assign (y (- 0 x))) (assign (y x)))
    :precondition true
    :postcondition (= y (abs (old x))))

(declare-const x Int)
(declare-const y Int)

(push)
(assert-counterexample true
    (call abs-proc (x) (x))
    (= x (abs (old x))))
(set-info :status unsat)
(check-sat)
(pop)
