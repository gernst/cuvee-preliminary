;! Cuvee -z3

(define-class Counter ((counter Int))
    (init () () (assign (counter 0)))
    (increment () () (assign (counter (+ counter 1))))
    (get-count () ((count Int)) (assign (count counter))))

(define-class NegativeCounter ((counter Int))
    (init () () (assign (counter 0)))
    (increment () () (assign (counter (- counter 1))))
    (get-count () ((count Int)) (assign (count (- 0 counter)))))

(verify-refinement (Counter (c Int)) (NegativeCounter (n Int)) (= c (- 0 n)))

(check-sat :expect unsat)