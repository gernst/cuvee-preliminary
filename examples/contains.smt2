; (set-option :produce-models true)
(declare-sort Elem)
(declare-const A (Array Int Elem))

(declare-const x Elem)
(declare-const r Bool)
(declare-const i Int)
(declare-const n Int)

(define-fun
    contains ((x Elem) (m Int) (n Int) (A (Array Int Elem))) Bool
        (exists ((i Int))
            (and (<= m i) (< i n) (= (select A i) x))))

(assert-counterexample
    (and (<= 0 i) (<= i n))
    (while
        (< i n)
        (block
        (if (= (select A i) x)
            (assign (r true)))
        (assign (i (+ i 1)))))
    (and (= i n)
         (= r (or (old r) (contains x (old i) n A)))) ; or (old r) ???
    )

(check-sat)
