;! Cuvee -z3

(set-logic ALL)

; a very stupid way to create zero
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

(push)
(verify-proc zero-proc)
(check-sat :expect unsat)
(pop)

(declare-const x Int)
(declare-const y Int)

(push)
(assert-counterexample
    (>= x 0)
    (call zero-proc (x) (y))
    (= y 0))
(check-sat :expect unsat)
(pop)

; the same function, but uses the same variable name to return the value
(define-proc zero-proc-inplace ((x Int)) ((x Int))
    (while
        (> x 0)
        (assign (x (- x 1)))
        :termination x
        :precondition (>= x 0)
        :postcondition (= x 0))
    :precondition (>= x 0)
    :postcondition (= x 0))

(push)
(verify-proc zero-proc-inplace)
(check-sat :expect unsat)
(pop)