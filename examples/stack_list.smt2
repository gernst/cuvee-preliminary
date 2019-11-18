(declare-sort Elem)
(declare-datatypes
    ((Lst 0))
    (((cons (head Elem) (tail Lst))
      (nil))))

(declare-fun xs () Lst)
(declare-fun xs1 () Lst)
(declare-fun in () Elem)
(declare-fun out () Elem)

; isempty

(assert (= xs1 nil))

; push

(assert (= xs1 (cons in xs)))

; pop

(assert (and (distinct xs nil) (= xs (cons out xs1))))