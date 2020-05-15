;! Cuvee -format -z3
; This a simple example where the refinement can simply be abducted

(define-class Counter ((counter Int))
    (init () () (assign (counter 0)))
    (increment () () (assign (counter (+ counter 1))))
    (get-count () ((count Int)) (assign (count counter))))

(define-class NegativeCounter ((neg Int))
    (init () () (assign (neg 0)))
    (increment () () (assign (neg (- neg 1))))
    (get-count () ((count Int)) (assign (count (- 0 neg)))))

(verify-refinement Counter NegativeCounter R :synthesize abduce)

(set-info :status unsat)
(check-sat)
