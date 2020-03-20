(declare-sort Elem)

(define-fun
    contains ((x Elem) (m Int) (n Int) (A (Array Int Elem))) Bool
        (exists ((i Int))
            (and (<= m i) (< i n) (= (select A i) x))))

(define-proc
    CONTAINS
        ((x Elem) (i Int) (n Int) (A (Array Int Elem)))
        ((r Bool))
        (if (< i n)
            (if (= (select A i) x)
                (assign (r true))
                (call CONTAINS (x (+ i 1) n A) (r)))
            (assign (r false)))
        :precondition  (<= i n)
        :postcondition (= r (contains x i n A)))

(verify-proc CONTAINS)
(check-sat)
