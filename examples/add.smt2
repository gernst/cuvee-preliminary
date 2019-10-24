(set-logic ALL)

(declare-const x Int)
(declare-const y Int)

(assert-counterexample
   (and (>= x 0) (>= y 0))
   (while (> x 0)
          (assign (x (- x 1))
                  (y (+ y 1)))
          :termination x)
    (= y (+ (old x) (old y))))

(check-sat)
