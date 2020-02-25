(push 1)
(assert (not (and
    (forall ((c Int) (n Int)) (=> (and true true) (= 0 (- 0 0))))
    (forall ((c Int) (n Int)) (=> true (=> (and true (= c (- 0 n))) (and true (and true (forall ((n1 Int)) (=> (= n1 (- n 1)) (and true (= (+ c 1) (- 0 n1))))))))))
    (forall ((c Int) (count Int) (n Int) (|count'| Int)) (=> true (=> (and true (= c (- 0 n))) (and true (and (= c (- 0 n)) (= c (- 0 n))))))))))
(check-sat)
(pop 1)
