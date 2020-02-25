;! Cuvee -z3

(set-logic ALL)

; this is an example where the relation just drops out of the proof

(define-class simple-account ((balance Int))
    (init () () (assign (balance 0)))
    (deposit ((amount Int)) ((new-balance Int)) (block
            (assign (balance (+ balance amount)))
            (assign (new-balance balance)))
        :precondition (> amount 0))
    (withdraw ((amount Int)) ((new-balance Int)) (block
            (assign (balance (- balance amount)))
            (assign (new-balance balance)))
        :precondition (and (> amount 0) (<= amount balance))))

(define-class double-account ((debit Int) (credit Int) (overdraft-limit Int))
    (init ((od-lim Int)) () (assign
            (overdraft-limit od-lim)
            (credit 0)
            (debit 0))
        :precondition (>= od-lim 0))

    ; use different argument names here to make sure the right variables names are used
    (deposit ((add Int)) ((increased Int)) (block
            (assign (credit (+ credit add)))
            (assign (increased (- credit debit))))
        :precondition (> amount 0))

    ; use different argument names here to make sure the right variables names are used
    (withdraw ((remove Int)) ((decreased Int)) (block
            (assign (debit (+ debit remove)))
            (assign (decreased (- credit debit))))
        :precondition (and (> amount 0) (<= amount (+ (- credit debit) overdraft-limit)))))

; we're using aliases for all state variables to make sure that they're renamed in all places
(verify-refinement (simple-account (b Int)) (double-account (d Int) (c Int) (l Int)) (and
        (= b (- c d))
        (>= l 0)))

(check-sat :expect unsat)
