(set-logic ALL)
(declare-const x Int)
(declare-const y Int)

(push)
(assert
  (not
    (=> (and (<= 0 x) (<= 0 y))
        (wp (assign (x (+ x y)))
            (>= x y)))))
(check-sat)
(pop)

(push)
(assert
  (not
    (=> (and (<= 0 x) (<= 0 y))
        (wp (assign (x (+ x y)))
            (< x y)))))
(check-sat)
(get-model)
(pop)