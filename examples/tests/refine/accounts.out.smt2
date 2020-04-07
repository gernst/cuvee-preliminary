(set-logic ALL)
(declare-fun overdraft-limit () Int)
(push 1)
(assert
  (>= overdraft-limit 0))
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
        ((b Int) (amount Int) (d Int) (c Int) (|add'| Int))
        (=>
          (and
            (= |add'| amount)
            (> amount 0)
            (= b (- c d)))
          (and
            (> |add'| 0)
            (= (- (+ c |add'|) d) (+ b amount))
            (= (+ b amount) (- (+ c |add'|) d)))))
      (forall
        ((b Int) (amount Int) (d Int) (c Int) (|remove'| Int))
        (=>
          (and
            (= |remove'| amount)
            (> amount 0)
            (<= amount b)
            (= b (- c d)))
          (and
            (> |remove'| 0)
            (<= |remove'| (+ (- c d) overdraft-limit))
            (= (- c (+ d |remove'|)) (- b amount))
            (= (- b amount) (- c (+ d |remove'|)))))))))
(check-sat)
(pop 1)
