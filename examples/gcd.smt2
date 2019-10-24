(set-logic ALL)

(declare-const m Int)
(declare-const n Int)

(declare-fun gcd (Int Int) Int)

(assert (forall ((m Int)) (= (gcd m m) m)))
(assert (forall ((m Int) (n Int)) (=> (< m n) (= (gcd m n) (gcd m (- n m))))))
(assert (forall ((m Int) (n Int)) (=> (> m n) (= (gcd m n) (gcd (- m n) n)))))

(assert-counterexample
  (and (< 0 m) (< 0 n))
  (while (not (= m n))
         (if (< m n) (assign (n (- n m)))
                     (assign (m (- m n))))
          :termination (+ m n))
  (and (< 0 m)
       (= m (gcd (old m) (old n)))))

(check-sat)
