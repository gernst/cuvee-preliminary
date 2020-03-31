(set-logic ALL)

(declare-sort Elem 0)
(declare-fun pre   (Elem Elem) Bool)
(declare-fun post  (Elem Elem Elem) Bool)

(declare-fun test  (Elem Elem) Bool)
(declare-fun body  (Elem Elem) Elem)

(declare-const x Elem)
(declare-const y Elem)
(assert
  (forall ((x1 Elem))
    (=> (and (not (test x1 y)) (pre x1 y))
        (post x1 x1 y))))
(assert
  (forall ((x1 Elem))
    (=> (and (test x1 y) (pre x1 y))
        (and (pre (body x1 y) y)
             (forall ((x2 Elem))
               (=> (post (body x1 y) x2 y)
                   (post x1 x2 y)))))))

(assert-counterexample
  (pre x y)
  (while (test x y)
    (assign (x (body x y))))
  (post (old x) x y))

(check-sat)
