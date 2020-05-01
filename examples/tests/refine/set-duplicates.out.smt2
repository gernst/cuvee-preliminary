(set-logic ALL)
(declare-sort Elem 0)
(declare-datatypes ((Lst 0)) (((cons (head Elem) (tail Lst)) (nil))))
(declare-fun values0 () (Array Elem Bool))
(declare-fun contains (Elem Lst) Bool)
(declare-fun removeall (Elem Lst) Lst)
(define-fun R ((xs Lst) (values (Array Elem Bool))) Bool 
  (forall
    ((x Elem))
    (=
      (contains x xs)
      (select values x))))
(push 1)
(assert
  (forall
    ((x Elem))
    (=
      (select values0 x)
      false)))
(assert
  (forall
    ((x Elem))
    (=
      (contains x nil)
      false)))
(assert
  (forall
    ((x Elem) (y Elem) (xs Lst))
    (=
      (contains x (cons y xs))
      (or
        (= x y)
        (contains x xs)))))
(assert
  (forall
    ((x Elem))
    (=
      (removeall x nil)
      nil)))
(assert
  (forall
    ((x Elem) (y Elem) (xs Lst))
    (=
      (removeall x (cons y xs))
      (ite (= x y) (removeall x xs) (cons y (removeall x xs))))))
(assert
  (forall
    ((x Elem) (y Elem) (xs Lst))
    (=
      (contains x (removeall y xs))
      (and
        (distinct x y)
        (contains x xs)))))
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
          (and true
               (R nil values0))))
      (forall
        ((xs Lst) (x Elem) (values (Array Elem Bool)) (|x'| Elem))
        (=>
          (and
            (= |x'| x)
            true
            (R xs values))
          (and
            true
            (and true
                 (R (cons x xs) (store values |x'| true))))))
      (forall
        ((xs Lst) (x Elem) (values (Array Elem Bool)) (|x'| Elem))
        (=>
          (and
            (= |x'| x)
            true
            (R xs values))
          (and
            true
            (and true
                 (R (removeall x xs) (store values |x'| false))))))
      (forall
        ((xs Lst) (x Elem) (values (Array Elem Bool)) (|x'| Elem))
        (=>
          (and
            (= |x'| x)
            true
            (R xs values))
          (and
            true
            (and (= (select values |x'|) (contains x xs))
                 (R xs values))))))))
(check-sat)
(pop 1)
