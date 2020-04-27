(set-logic ALL)

(declare-sort Elem)

(declare-datatypes
    ((Lst 0))
    (((cons (head Elem) (tail Lst))
      (nil))))

(declare-const values0 (Array Elem Bool))
(assert (forall ((x Elem)) (= (select values0 x) false)))

(declare-fun contains (Elem Lst) Bool)
(assert (forall ((x Elem))
    (= (contains x nil) false)))
(assert (forall ((x Elem) (y Elem) (xs Lst))
    (= (contains x (cons y xs))
       (or (= x y) (contains x xs)))))

(declare-fun removeall (Elem Lst) Lst)
(assert (forall ((x Elem))
    (= (removeall x nil) nil)))
(assert (forall ((x Elem) (y Elem) (xs Lst))
    (= (removeall x (cons y xs))
       (ite (= x y)
            (removeall x xs)
            (cons y (removeall x xs))))))

; lemma
(assert (forall ((x Elem) (y Elem) (xs Lst))
    (= (contains x (removeall y xs))
       (and (distinct x y)
            (contains x xs)))))

(define-class
  ListSet
  ((xs Lst))
  (init () ()
    (assign (xs nil)))
  (add ((x Elem)) ()
    (assign (xs (cons x xs))))
  (del ((x Elem)) ()
    (assign (xs (removeall x xs))))
  (has ((x Elem)) ((y Bool))
    (assign (y (contains x xs)))))

(define-class
  ArraySet
  ((values (Array Elem Bool)))
  (init () ()
    (assign (values values0)))
  (add ((x Elem)) ()
    (assign ((select values x) true)))
  (del ((x Elem)) ()
    (assign ((select values x) false)))
  (has ((x Elem)) ((y Bool))
    (assign (y (select values x)))))

(verify-refinement
   ListSet ArraySet R :synthesize output)

(set-info :status unsat)
(check-sat)


