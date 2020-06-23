;! Cuvee -format -z3

(declare-sort Elem)

(define-class Cell ((value Elem))
  (init () ())
  (set ((newValue Elem)) () (assign (value newValue))
    :postcondition (= value newValue))
  (get () ((out Elem)) (assign (out value))
    :postcondition (= out value)))

(verify-class Cell)
(set-info :status unsat)
(check-sat)