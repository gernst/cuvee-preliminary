(set-logic ALL)
(declare-fun p () Bool)
(declare-fun q () Bool)
(declare-fun r () Bool)
(push 1)
(assert p)
(assert r)
(assert q)
(check-sat)
(pop 1)
