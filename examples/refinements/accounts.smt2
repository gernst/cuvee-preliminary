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
    (deposit ((amount Int)) ((new-balance Int)) (block
            (assign (credit (+ credit amount)))
            (assign (new-balance (- credit debit))))
        :precondition (> amount 0))
    (withdraw ((amount Int)) ((new-balance Int)) (block
            (assign (debit (+ debit amount)))
            (assign (new-balance (- credit debit))))
        :precondition (and (> amount 0) (<= amount (+ (- credit debit) overdraft-limit)))))

; balance = credit - debit comes immediately out of the output equality
; overdraft-limit >= 0 is also required for the proof. Maybe it can be extracted.

(refinement (as simple-account) (cs double-account) (and
        (= as_balance (- cs_credit cs_debit))
        (>= cs_overdraft-limit 0)))