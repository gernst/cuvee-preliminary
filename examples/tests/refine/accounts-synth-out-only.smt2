;! Cuvee -z3

(set-logic ALL)

; this is an example where the relation just drops out of the proof

(define-class SimpleAccount ((balance Int))
    (init () () (assign (balance 0)))
    (deposit ((amount Int)) ((new-balance Int)) (block
            (assign (balance (+ balance amount)))
            (assign (new-balance balance)))
        :precondition (> amount 0))
    (withdraw ((amount Int)) ((new-balance Int)) (block
            (assign (balance (- balance amount)))
            (assign (new-balance balance)))
        :precondition (and (> amount 0) (<= amount balance))))

(define-class DoubleAccount ((debit Int) (credit Int))
    (init () () (assign
            (credit 0)
            (debit 0)))

    ; use different argument names here to make sure the right variables names are used
    (deposit ((add Int)) ((increased Int)) (block
            (assign (credit (+ credit add)))
            (assign (increased (- credit debit))))
        :precondition (> add 0))

    ; use different argument names here to make sure the right variables names are used
    (withdraw ((remove Int)) ((decreased Int)) (block
            (assign (debit (+ debit remove)))
            (assign (decreased (- credit debit))))
        :precondition (and (> remove 0) (<= remove (- credit debit)))))

(declare-fun R (Int Int Int) Bool)

(verify-refinement SimpleAccount DoubleAccount R :synthesize output)

(set-info :status unsat)
(check-sat)
