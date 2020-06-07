(push)
(assert (not (forall ((x Int)) (= x x))))
(check-sat)
(pop)