(set-logic ALL)
(declare-fun overdraft-limit () Int)
(push 1)
(assert
  (>= overdraft-limit 0))
(assert
  (not
    (and
      (forall
        ((b Int) (d Int) (c Int))
        (=>
          (and
            true
            true
            true)
          (and
            true
            true
            (= 0 (- 0 0)))))
      (forall
        ((b Int) (amount Int) (new-balance Int) (d Int) (c Int) (|add'| Int) (|increased'| Int))
        (=>
          (and
            (= amount |add'|)
            (> amount 0)
            (= b (- c d)))
          (and
            (> |add'| 0)
            (= (+ b amount) (- (+ c |add'|) d))
            (= (+ b amount) (- (+ c |add'|) d)))))
      (forall
        ((b Int) (amount Int) (new-balance Int) (d Int) (c Int) (|remove'| Int) (|decreased'| Int))
        (=>
          (and
            (= amount |remove'|)
            (> amount 0)
            (<= amount b)
            (= b (- c d)))
          (and
            (> |remove'| 0)
            (<= |remove'| (+ (- c d) overdraft-limit))
            (= (- b amount) (- c (+ d |remove'|)))
            (= (- b amount) (- c (+ d |remove'|)))))))))
(check-sat)
(pop 1)
