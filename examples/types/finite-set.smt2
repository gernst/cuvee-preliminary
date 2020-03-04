(declare-sort Elem)

(declare-sort FinSet)

(declare-const empty FinSet)
(declare-fun   size (FinSet) Int)
(declare-fun   contains (FinSet Elem) Bool)
(declare-fun   insert   (FinSet Elem) FinSet)
(declare-fun   remove   (FinSet Elem) FinSet)

; axioms for size
(assert
  (= (size empty) 0))
(assert (forall ((s FinSet) (x Elem))
  (= (size (insert s x))
     (ite (contains s x)
          (size s) ; don't count duplicate inserts
          (+ (size s) 1)))))
  
; axioms for contains
(assert (forall ((y Elem))
  (= (contains empty y)
     false)))
(assert (forall ((s FinSet) (y Elem) (x Elem))
  (= (contains (insert s x) y)
     (or (= x y) (contains s y)))))

; axioms for remove
(assert (forall ((y Elem))
  (= (remove empty y)
     empty)))
(assert (forall ((s FinSet) (y Elem) (x Elem))
  (= (remove (insert s x) y)
     (ite (= x y)
          (remove s y)
          (insert (remove s y) x)))))

; extensionality
(assert (forall ((s1 FinSet) (s2 FinSet))
  (=> (forall ((x Elem))
        (= (contains s1 x) (contains s2 x)))
      (= s1 s2))))
