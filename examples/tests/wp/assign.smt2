;! Cuvee -format -z3

(assert (not (forall ((x Int) (y Int))
  (wp (assign (y x)) (= x y)))))

(set-info :status unsat)
(check-sat)
