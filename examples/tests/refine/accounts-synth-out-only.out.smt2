(set-logic ALL)
(declare-fun R (Int Int Int) Bool)
(push 1)
(assert (forall ((balance Int) (debit Int) (credit Int)) (= (R balance debit credit) (= (+ balance debit) credit))))
(assert (not (and (and (forall ((balance Int) (debit Int) (credit Int)) (=> (and (and true true) true) (and true (and true (R 0 0 0))))) (forall ((balance Int) (amount Int) (new-balance Int) (debit Int) (credit Int) (|add'| Int) (|increased'| Int)) (=> (and (and (= amount |add'|) (> amount 0)) (R balance debit credit)) (and (> |add'| 0) (and (= (+ balance amount) (- (+ credit |add'|) debit)) (R (+ balance amount) debit (+ credit |add'|))))))) (forall ((balance Int) (amount Int) (new-balance Int) (debit Int) (credit Int) (|amount'| Int) (|decreased'| Int)) (=> (and (and (= amount |amount'|) (and (> amount 0) (<= amount balance))) (R balance debit credit)) (and (and (> |amount'| 0) (<= |amount'| (- credit debit))) (and (= (- balance amount) (- credit (+ debit |amount'|))) (R (- balance amount) (+ debit |amount'|) credit))))))))
(check-sat)
(pop 1)
