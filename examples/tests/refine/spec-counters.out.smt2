(push 1)
(assert
  (not
    (and
      (=>
        (and
          true
          true
          true)
        (and
          true
          true
          (= 0 (- 0 0))))
      (forall
        ((c Int) (n Int))
        (=>
          (and
            true
            true
            (= c (- 0 n)))
          (and
            true
            true
            (forall
              ((n1 Int))
              (=>
                (= n1 (- n 1))
                (and
                  true
                  (= (+ c 1) (- 0 n1))))))))
      (forall
        ((c Int) (n Int))
        (=>
          (and
            true
            true
            (= c (- 0 n)))
          (and
            true
            (= (- 0 n) c)
            (= c (- 0 n))))))))
(check-sat)
(pop 1)
