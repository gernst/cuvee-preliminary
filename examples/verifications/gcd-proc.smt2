(set-logic ALL)

(declare-fun gcd (Int Int) Int)

(assert (forall ((m Int)) (= (gcd m m) m)))
(assert (forall ((m Int) (n Int)) (=> (< m n) (= (gcd m n) (gcd m (- n m))))))
(assert (forall ((m Int) (n Int)) (=> (> m n) (= (gcd m n) (gcd (- m n) n)))))

(define-proc find-gcd ((m Int) (n Int)) ((g Int))
  (block
     (while (distinct m n)
              (if (< m n) (assign (n (- n m)))
                          (assign (m (- m n))))
               :termination (+ m n)
               :precondition (and (< 0 m) (< 0 n))
               :postcondition (and (< 0 m) (= m (gcd (old m) (old n)))))
     (assign (g m))
  )
  :precondition (and (< 0 m) (< 0 n))
  :postcondition (and (< 0 g) (= g (gcd (old m) (old n))))
)

(define-proc find-gcd-rec ((m Int) (n Int)) ((m Int))
    (if (distinct m n)
             (if (< m n) (call find-gcd-rec (m (- n m)) (m))
                         (call find-gcd-rec ((- m n) n) (m))))
  :precondition (and (< 0 m) (< 0 n))
  :postcondition (and (< 0 m) (= m (gcd (old m) (old n))))
)
