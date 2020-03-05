;! Cuvee -z3

(set-logic ALL)

; this is an example where the relation just drops out of the proof

; can't have this as an argument of initialization in the current theory
; for now, let's model it as an arbitrary global constant with some properties
(declare-const overdraft-limit Int)
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
        :precondition (> amount 0))

    ; use different argument names here to make sure the right variables names are used
    (withdraw ((remove Int)) ((decreased Int))
            (assign (debit (+ debit remove)))
            (assign (decreased (- credit debit)))
; BUG BUG BUG "amount" is allowed to refer to the abstract precondition here, lol
        :precondition (and (> amount 0) (<= amount (+ (- credit debit) overdraft-limit)))))

; we're using aliases for all state variables to make sure that they're renamed in all places
(verify-refinement (SimpleAccount (b Int)) (DoubleAccount (d Int) (c Int))
   (= b (- c d)))

(set-info :status unsat)
(check-sat)
