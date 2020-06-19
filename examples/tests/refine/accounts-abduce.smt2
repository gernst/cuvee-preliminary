;! Cuvee -format -z3

(set-logic ALL)

; this is an example where the relation just drops out of the proof

; can't have this as an argument of initialization in the current theory
; for now, let's model it as an arbitrary global constant with some properties
(declare-const overdraft-limit Int)

; Unfortunately, we can't abduce the following cleanly. We get
; (forall ((amount Int))
;   (or (<= amount 0) (< balance amount)
;     (<= (+ amount debit) (+ credit overdraft-limit))))))
; which can be simplified to (>= overdraft-limit 0) using (= credit (+ balance debit))
; but that requires linear math which is more elaborate than what is currently implemented.
(assert (>= overdraft-limit 0))
  
(define-class SimpleAccount ((balance Int))
    (init () () (assign (balance 0)))
    (deposit ((amount Int)) ((new-balance Int))
            (assign (balance (+ balance amount)))
            (assign (new-balance balance))
        :precondition (> amount 0))
    (withdraw ((amount Int)) ((new-balance Int))
            (assign (balance (- balance amount)))
            (assign (new-balance balance))
        :precondition (and (> amount 0) (<= amount balance))))

(define-class DoubleAccount ((debit Int) (credit Int))
    (init () ()
      (assign (credit 0) (debit 0)))

    ; use different argument names here to make sure the right variables names are used
    (deposit ((add Int)) ((increased Int))
            (assign (credit (+ credit add)))
            (assign (increased (- credit debit)))
        :precondition (> add 0))

    ; use different argument names here to make sure the right variables names are used
    (withdraw ((remove Int)) ((decreased Int))
            (assign (debit (+ debit remove)))
            (assign (decreased (- credit debit)))
        :precondition (and (> remove 0) (<= remove (+ (- credit debit) overdraft-limit)))))

(verify-refinement SimpleAccount DoubleAccount R :synthesize abduce)

(set-info :status unsat)
(check-sat)
