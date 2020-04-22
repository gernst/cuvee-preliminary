(set-logic ALL)
(define-fun R  ((balance Int) (debit Int) (credit Int)) Bool
  (= (+ balance debit) credit))
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
          (R 0 0 0)))
      (forall
        ((balance Int) (amount Int) (debit Int) (credit Int) (|add'| Int))
        (=>
          (and
            (= |add'| amount)
            (> amount 0)
            (R balance debit credit))
          (and
            (> |add'| 0)
            (= (- (+ credit |add'|) debit) (+ balance amount))
            (R (+ balance amount) debit (+ credit |add'|)))))
      (forall
        ((balance Int) (amount Int) (debit Int) (credit Int) (|amount'| Int))
        (=>
          (and
            (= |amount'| amount)
            (> amount 0)
            (<= amount balance)
            (R balance debit credit))
          (and
            (> |amount'| 0)
            (<= |amount'| (- credit debit))
            (= (- credit (+ debit |amount'|)) (- balance amount))
            (R (- balance amount) (+ debit |amount'|) credit)))))))
(check-sat)
(pop 1)
