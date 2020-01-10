(set-logic ALL)

(define-proc abs-proc ((x Int)) ((y Int))
    (if (< x 0) (assign (y (- 0 x))) (assign (y x)))
    :precondition true
    :postcondition (= y (abs (old x))))