(set-logic ALL)
(push 1)
(assert
  (not
    (forall
      ((x Int) (y Int) (z Int))
      (=>
        true
        (and
          (=>
            (< x y)
            (and
              (>= y x)
              (>= y y)))
          (=>
            (not
              (< x y))
            (and
              (>= x x)
              (>= x y))))))))
(check-sat)
(pop 1)
