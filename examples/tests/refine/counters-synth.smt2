;! Cuvee -format -z3

(define-class Counter ((counter Int))
    (init () () (assign (counter 0)))
    (increment () () (assign (counter (+ counter 1))))
    (get-count () ((count Int)) (assign (count counter))))

(define-class NegativeCounter ((neg Int))
    (init () () (assign (neg 0)))
    (increment () () (assign (neg (- neg 1))))
    (get-count () ((count Int)) (assign (count (- 0 neg)))))

(declare-fun R (Int Int) Bool)

(verify-refinement Counter NegativeCounter R :synthesize output)

(set-info :status unsat)
(check-sat)
