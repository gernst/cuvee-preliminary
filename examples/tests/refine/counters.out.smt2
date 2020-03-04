(push 1)
(assert (not (and (forall ((c Int) (n Int)) (=> (and true true true) (and true (and true (= 0 (- 0 0)))))) (forall ((c Int) (n Int)) (=> (and true true (= c (- 0 n))) (and true (and true (= (+ c 1) (- 0 (- n 1))))))) (forall ((c Int) (count Int) (n Int) (|count'| Int)) (=> (and true true (= c (- 0 n))) (and true (and (= c (- 0 n)) (= c (- 0 n)))))))))
(check-sat)
(pop 1)
