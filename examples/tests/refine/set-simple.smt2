;! Cuvee -z3

(set-logic ALL)

(declare-sort Elem)

(declare-datatypes
    ((Lst 0))
    (((cons (head Elem) (tail Lst))
      (nil))))

(declare-const
  empty (Array Elem Bool))
(assert (forall ((x Elem))
  (= (select empty x) false)))

(declare-fun
  contains (Elem Lst) Bool)
(assert (forall ((x Elem))
  (= (contains x nil) false)))
(assert (forall ((x Elem) (y Elem) (xs Lst))
  (= (contains x (cons y xs)) (or (= x y) (contains x xs)))))

(define-class
  ListSet
  ((xs Lst))
  (init () ()
  	(assign (xs nil)))
  (insert ((x Elem)) ()
    (assign (xs (cons x xs))))
  (contains ((x Elem)) ((r Bool))
    (assign (r  (contains x xs)))))

(define-class
  ArraySet
  ((elems (Array Elem Bool)))
  (init () ()
    (assign (elems empty)))
  (insert ((x Elem)) ()
    (assign (elems (store elems x true))))
  (contains ((x Elem)) ((r Bool))
    (assign (r (select elems x)))))

(declare-fun R
  (Lst (Array Elem Bool))
  Bool)

(assert (forall ((xs Lst) (elems (Array Elem Bool)))
  (= (R xs elems)
     (forall ((x Elem)) (= (contains x xs) (select elems x))))))

(verify-refinement
   ListSet ArraySet R)

(set-info :status unsat)
(check-sat)

