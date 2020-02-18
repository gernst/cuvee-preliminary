(set-logic ALL)

(declare-const m Int)
(declare-const n Int)

(declare-fun gcd (Int Int) Int)

(assert (forall ((m Int)) (= (gcd m m) m)))
(assert (forall ((m Int) (n Int)) (=> (< m n) (= (gcd m n) (gcd m (- n m))))))
(assert (forall ((m Int) (n Int)) (=> (> m n) (= (gcd m n) (gcd (- m n) n)))))

(check-sat)
