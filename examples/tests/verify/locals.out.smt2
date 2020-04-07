(push 1)
(assert
  (not
    (forall
      ((x Int))
      (=>
        true
        (= x x)))))
(check-sat)
(pop 1)
(push 1)
(assert
  (not
    (forall
      ((x Int))
      (=>
        true
        (= x x)))))
(assert
  (not
    (forall
      ((z Int))
      (=>
        true
        (and
          (exists
            ((x1 Int))
            (>= x1 0))
          (forall
            ((x1 Int))
            (=>
              (>= x1 0)
              (>= (+ x1 z) x1))))))))
(check-sat)
(pop 1)
