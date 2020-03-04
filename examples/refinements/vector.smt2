(declare-sort Elem)
(declare-datatypes
    ((Lst 0))
    (((cons (head Elem) (tail Lst))
      (nil))))

(declare-fun len (Lst) Int)
(declare-fun get (Int Lst) Elem)
(declare-fun remove (Int Lst) Lst)
(declare-fun insert (Int Elem Lst) Lst)

; definition of list length
(assert (forall ((xs Lst)) (= (len nil) 0))) ;  using forall just to parse
(assert (forall ((x Elem) (xs Lst)) (= (len (cons x xs)) (+ 1 (len xs)))))

; lemma: len is positive
(assert (forall ((xs Lst)) (>= (len xs) 0)))

; positional get
(assert (forall ((x Elem) (xs Lst))
        (= (get 0 (cons x xs)) x)))
(assert (forall ((i Int) (x Elem) (xs Lst))
    (=> (> i 0)
        (= (get i (cons x xs)) (get (- i 1) xs)))))

; positional remove
(assert (forall ((x Elem) (xs Lst))
        (= (remove 0 (cons x xs)) xs)))
(assert (forall ((i Int) (x Elem) (xs Lst))
    (=> (> i 0)
        (= (remove i (cons x xs)) (cons x (remove (- i 1) xs))))))

; positional insert
(assert (forall ((y Elem) (xs Lst))
        (= (insert 0 y xs) (cons y xs))))
(assert (forall ((i Int) (y Elem) (x Elem) (xs Lst))
    (=> (> i 0)
        (= (insert i y (cons x xs)) (cons x (insert (- i 1) y xs))))))

; lemma for len after insert
(assert (forall ((i Int) (y Elem) (xs Lst))
    (=> (<= i (len xs))
        (= (len (insert i y xs)) (+ (len xs) 1)))))

; lemma for get after insert
(assert (forall ((i Int) (j Int) (y Elem) (xs Lst))
    (=> (and (< i (len xs)) (< j (len xs)))
             (= (get i (insert j y xs))
                (ite (< i j) (get i xs)
                             (ite (= i j) y (get (- i 1) xs)))))))

(define-class
  ListVector
  ((xs Lst))
  (init () ()
    (assign (xs nil)))
  (lookup ((i Int)) ((o Elem))
    (assign (o (get i xs)))
    :precondition (< i (len xs)))
  (insert ((i Int) (z Elem)) ()
    (assign (xs (insert i z xs)))
    :precondition (<= i (len xs)))
  (remove ((i Int)) ()
    (assign (xs (remove i xs)))
    :precondition (< i (len xs))))

(define-class
  ArrayVector
  ((size Int) (values (Array Int Elem)))
  (init () ()
    (assign (size 0)))
  (lookup ((i Int)) ((o Elem))
    (assign (o (select values i)))
    :precondition (< i size))
  (insert ((i Int) (z Elem)) ()
    (spec (values)
      true
      (and
        ; the value itself
        (= (select values i) z)

        ; indices in prefix
        (forall ((k Int))
            (=> (< k i)
                    (= (select values k) (select (old values) k))))
    
        ; indices in suffix
        (forall ((k Int))
            (=> (and (<= i k) (< k size))
                    (= (select values (+ k 1)) (select (old values) k))))))
    :precondition (<= i (len xs)))
  (remove ((i Int)) ()
    (block)
    :precondition (< i (len xs))))

(declare-fun R (Lst Int (Array Int Elem)) Bool)

(assert (forall ((xs Lst) (n Int) (A (Array Int Elem)))
    (= (R xs n A)
       (and (= (len xs) n)
            (forall ((i Int))
                (=> (< i n) (= (select A i) (get i xs))))))))

(verify-refinement
  ListVector ArrayVector R)
(check-sat)
