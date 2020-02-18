(set-logic ALL)

; compute the maximum

(declare-const x Int)
(declare-const y Int)

(declare-const a (Array Int Int))

(assert-counterexample
  (<= x y)
  (while (not (= x y))
         (if (<= (select a x) (select a y))
             (assign (x (+ x 1)))
             (assign (y (- y 1))))
         :termination (- y x))
  (forall ((z Int))
    (=> (and (<= (old x) z)
             (<= z (old y)))
        (<= (select a z) (select a x)))))

(check-sat)