(set-logic ALL)

(declare-sort Elem 0)
(declare-sort Lst 0)

(declare-const dash Elem)

(declare-const nil Lst)
(declare-fun   cons (Elem Lst) Lst)

(assert (forall ((x Elem) (xs Lst))
    (not (= nil (cons x xs)))))

(declare-fun rmdash (Lst) Lst)
(assert
    (= (rmdash nil)
       nil))
(assert (forall ((x Elem) (xs Lst))
    (= (rmdash (cons x xs))
       (ite (= x dash) (rmdash xs)
                       (cons x (rmdash xs))))))

(declare-fun tolist ((Array Int Elem) Int Int) Lst)
(assert (forall ((a (Array Int Elem)) (n Int))
  (= (tolist a n n)
     nil)))
(assert (forall ((a (Array Int Elem)) (i Int) (n Int))
  (=> (< i n)
      (= (tolist a i n)
         (cons (select a i) (tolist a (+ i 1) n))))))

(declare-const a (Array Int Elem))
(declare-const i Int)
(declare-const j Int)

(declare-const b (Array Int Elem))
(declare-const m Int)
(declare-const n Int)

(declare-const ok Bool)

(assert-counterexample
  (and (<= 0 i) (<= i m)
       (<= 0 j) (<= j n))
  (while true
    (if (and (= i m) (= j n))
      (block (assign (ok true))
             (break))
      (if (and (< i m) (= (select a i) dash))
        (block (assign (i (+ i 1))))
      (if (and (< j n) (= (select b j) dash))
        (block (assign (j (+ j 1))))
      (if (and (< i m) (< j n)
               (= (select a i) (select b j)))
        (block (assign (i (+ i 1))
                       (j (+ j 1))))
        (block (assign (ok false))
               (break))))))
    :termination (- (+ m n) (+ i j)))
  (= ok (= (rmdash (tolist a (old i) m))
           (rmdash (tolist b (old j) n)))))

(check-sat)
