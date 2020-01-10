(set-logic ALL)

(define-proc zero-proc ((x Int)) ((y Int))
    (block
        (while
            (> x 0)
            (assign (x (- x 1)))
            :termination x
            :precondition (>= x 0)
            :postcondition (= x 0))
        (assign (y x))
    )
    :precondition (>= x 0)
    :postcondition (= y 0))

(declare-const x Int)
(declare-const y Int)

(assert-counterexample
    (>= x 0)
    (call zero-proc (x) (y))
    (= y 0))

(check-sat)