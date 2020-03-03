(declare-sort Elem)

(declare-datatypes
    ((Lst 0))
    (((cons (head Elem) (tail Lst))
      (nil))))

(declare-fun
  contains (Elem Lst) Bool)
(assert (forall ((x Elem))
  (= (contains x nil) false)))
(assert (forall ((x Elem) (y Elem) (xs Lst))
  (= (contains x (cons y xs)) (or (= x y) (contains x xs)))))

(declare-fun snoc (Lst Elem) Lst)
(assert (forall ((x Elem))
  (= (snoc nil x) (cons x nil))))
(assert (forall ((y Elem) (xs Lst) (x Elem))
  (= (snoc (cons y xs) x) (cons y (snoc xs x)))))