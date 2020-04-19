;! Cuvee -simplify -z3

(set-logic ALL)

(declare-const p Bool)
(declare-const q Bool)
(declare-const r Bool)

(assert (and p (and p q r)))
(assert (=> (or p q) (=> p q)))

(set-info :status sat)
(check-sat)
