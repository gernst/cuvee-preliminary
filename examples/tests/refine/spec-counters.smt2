;! Cuvee -z3
; Works like counters.smt2 but the postcondition cannot be inlined because we use spec instead of assign.
; This forces renaming of the variables.

(define-class Counter ((counter Int))
    (init () () (assign (counter 0)))
    (increment () () (assign (counter (+ counter 1))))
    (get-count () ((count Int)) (assign (count counter))))

(define-class SpecCounter ((counter Int))
    (init () () (assign (counter 0)))
    (increment () () (spec (counter) true (= counter (- (old counter) 1))))
    (get-count () ((count Int)) (assign (count (- 0 counter)))))

(verify-refinement (Counter (c Int)) (SpecCounter (n Int)) (= c (- 0 n)))

(set-info :status unsat)
(check-sat)
