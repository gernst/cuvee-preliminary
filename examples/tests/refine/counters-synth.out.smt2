(define-fun R ((counter Int) (neg Int)) Bool
  (= counter (- neg)))
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
          (R 0 0)))
      (forall
        ((counter Int) (neg Int))
        (=>
          (and
            true
            true
            (R counter neg))
          (and
            true
            true
            (R (+ counter 1) (- neg 1)))))
      (forall
        ((counter Int) (neg Int))
        (=>
          (and
            true
            true
            (R counter neg))
          (and
            true
            (= (- 0 neg) counter)
            (R counter neg)))))))
(check-sat)
(pop 1)
