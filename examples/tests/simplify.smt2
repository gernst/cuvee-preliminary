;! Cuvee -simplify

(set-logic ALL)

(declare-const p Bool)
(declare-const q Bool)
(declare-const r Bool)

(assert (and p (and p q r)))
(assert (=> (or p q) (=> p q)))

(check-sat :expect unknown)