;! Cuvee -format -z3

(define-proc
  with-locals ((x Int)) ((y Int))
  (local (z Int))
  (assign (z x))
  (assign (y z))
  :postcondition (= x y))
  
(define-proc
  with-choose ((x Int)) ((y Int))
  (local (z Int))
  (choose (x) (>= x 0))
  (assign (y (+ x z)))
  :postcondition (>= y x))

(verify-proc with-locals)
(set-info :status unsat)
(check-sat)

(verify-proc with-choose)
(set-info :status unsat)
(check-sat)
