(set-logic ALL)

(declare-fun gcd (Int Int) Int)

(assert (forall ((m Int)) (= (gcd m m) m)))

(assert (not (forall ((m Int)) (= (gcd m m) m))))
(check-sat)
