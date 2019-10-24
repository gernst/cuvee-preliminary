(set-logic ALL)

(declare-sort  Elem 0)

(declare-const i Int)
(declare-const n Int)
(declare-const a (Array Int Elem))

(declare-fun f (Elem) Elem)

(assert-counterexample
  (and (<= 0 i) (<= i n))
  (while (< i n)
         (assign (a (store a i (f (select a i))))
                 (i (+ i 1)))
         :termination (- n i))
  (forall ((j Int))
    (= (select a j)
       (ite (and (<= (old i) j) (< j n))
            (f (select (old a) j))
               (select (old a) j)))))

(check-sat)