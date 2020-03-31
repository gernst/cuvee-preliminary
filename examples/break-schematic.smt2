(set-logic ALL)

(declare-sort Elem)

(declare-fun init  (Elem Elem) Bool)
(declare-fun pre   (Elem Elem) Bool)
(declare-fun guard (Elem Elem) Bool)
(declare-fun body  (Elem Elem) Elem)
(declare-fun post  (Elem Elem Elem) Bool)
(declare-fun fin   (Elem Elem Elem) Bool)

(define-const ok1 Bool
  (forall ((x Elem) (y Elem))
    (=> (init x y)
        (box (block
               (while true
                 (if (not (guard x y))
                    (break)
                    (assign (x (body x y))))
                 :precondition  (pre x y)
                 :postcondition (post (old x) x y))
               :save-old)
             (fin (old x) x y)))))
    
(define-const ok2 Bool
  (forall ((x Elem) (y Elem))
    (=> (init x y)
        (box (block
               (while (guard x y)
                    (assign (x (body x y)))
                 :precondition  (pre x y)
                 :postcondition (post (old x) x y))
               :save-old)
             (fin (old x) x y)))))

(define-const ok3 Bool
  (and
    (forall ((x Elem) (y Elem))
      (=> (init x y)
          (pre  x y)))
    (forall ((x0 Elem) (x Elem) (y Elem))
      (=> (and (init x0 y)
               (pre  x y)
               (not (guard x y)))
          (post x x y)))
    (forall ((x0 Elem) (x Elem) (y Elem) (z Elem))
      (=> (and (init x0 y)
               (pre  x y)
               (guard x y))
          (and (pre  (body x y) y)
               (=> (post (body x y) z y) (post x z y)))))
    (forall ((x0 Elem) (x Elem) (y Elem))
      (=> (and (init x0 y)
               (post x0 x y))
          (fin x0 x y)))))

(assert (not
    (and (= ok1 ok2)
         (= ok2 ok3))))
(check-sat)
