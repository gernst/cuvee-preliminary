(set-logic ALL)

(define-proc
  max ((x Int) (y Int)) ((z Int))
  (if (< x y) (assign (z y))
              (assign (z x)))
  :precondition  true
  :postcondition (and (>= z x) (>= z y)))

(verify-proc max)
