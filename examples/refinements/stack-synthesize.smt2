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

(verify-refinement
   ListStack ArrayStack R :synthesize)

(check-sat)

