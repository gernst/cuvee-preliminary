(push 1)
(assert (not (and (and (=> (and (and true true) true) (and true (and true (= 0 (- 0 0))))) (forall ((c Int) (n Int)) (=> (and (and true true) (= c (- 0 n))) (and true (and true (= (+ c 1) (- 0 (- n 1)))))))) (forall ((c Int) (n Int)) (=> (and (and true true) (= c (- 0 n))) (and true (and (= (- 0 n) c) (= c (- 0 n)))))))))
(check-sat)
(pop 1)
