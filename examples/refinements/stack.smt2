(set-logic ALL)

(declare-sort Elem)

(declare-datatypes
    ((Lst 0))
    (((cons (head Elem) (tail Lst))
      (nil))))

(define-class
  ListStack
  ((xs Lst))
  (init () ()
  	(assign (xs nil)))
  (push ((x Elem)) ()
    (assign (xs (cons x xs))))
  (pop  () ((x Elem))
    (assign (x  (head xs))
            (xs (tail xs)))
    :precondition
        (distinct xs nil)))

(define-class
  ArrayStack
  ((size Int) (values (Array Int Elem)))
  (init () ()
    (assign (size 0)))
  (push ((x Elem)) ()
    (assign (values (store values size x))
            (size   (+ size 1))))
  (pop  () ((x Elem))
    (assign (x      (select values (- size 1)))
            (size   (- size 1)))
    :precondition
        (> size 0)))

(declare-fun R
  (Lst Int (Array Int Elem))
  Bool)

(assert
  (forall
    ((head1 Elem) (tail2 Lst) (size Int) (values (Array Int Elem)))
    (=
      (R (cons head1 tail2) size values)
      (and
        (< 0 size)
        (= head1 (select values (- size 1)))
        (R tail2 (- size 1) values)))))
(assert
  (forall
    ((size Int) (values (Array Int Elem)))
    (=
      (R nil size values)
      (= 0 size))))

(assert
  (forall ((x Elem) (xs Lst) (m Int) (n Int) (values (Array Int Elem)))
    (=> (and (<= m n) (R xs m values))
                      (R xs m (store values n x)))))

(verify-refinement
   ListStack ArrayStack R)

(check-sat
  :expect unsat)

