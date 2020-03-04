;! Cuvee -z3

(set-logic ALL)

(declare-sort Elem)

(declare-datatypes
    ((Lst 0))
    (((cons (head Elem) (tail Lst))
      (nil))))

(declare-fun snoc (Lst Elem) Lst)
(assert (forall ((x Elem))
  (= (snoc nil x) (cons x nil))))
(assert (forall ((y Elem) (xs Lst) (x Elem))
  (= (snoc (cons y xs) x) (cons y (snoc xs x)))))

(define-class
  ListQueue
  ((xs Lst))
  (init () ()
  	(assign (xs nil)))
  (enq ((x Elem)) ()
    (assign (xs (snoc xs x))))
  (deq  () ((x Elem))
    (assign (x  (head xs))
            (xs (tail xs)))
    :precondition
        (distinct xs nil)))

(define-class
  ArrayQueue
  ((m Int)
   (n Int)
   (values (Array Int Elem)))
  (init () ()
    (assign (m n)))
  (enq ((x Elem)) ()
    (assign (values (store values n x))
            (n      (+ n 1))))
  (deq  () ((x Elem))
    (assign (x      (select values m))
            (m      (+ m 1)))
    :precondition
        (< m n)))

(declare-fun R
  (Lst Int Int (Array Int Elem))
  Bool)

(push)
(assert
  (forall
    ((m Int) (n Int) (values (Array Int Elem)))
    (=
      (R nil m n values)
      (and (= m n)))))

(assert
  (forall
    ((x Elem) (xs Lst) (m Int) (n Int) (values (Array Int Elem)))
    (=
      (R (cons x xs) m n values)
      (and
        (< m n)
        (= x (select values m))
        (R xs (+ m 1) n values)))))
(pop)

(verify-refinement
   ListQueue ArrayQueue R
   :synthesize consumer)

; (set-info :status unsat)
(check-sat)

