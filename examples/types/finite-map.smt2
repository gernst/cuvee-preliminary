(declare-sort Key)
(declare-sort Value)

(declare-sort Map)

(declare-const empty Map)
(declare-fun   size (Map) Int)
(declare-fun   contains (Map Key) Bool)
(declare-fun   get  (Map Key) Value)
(declare-fun   del  (Map Key) Map)
(declare-fun   put  (Map Key Value) Map)

; axioms for size
(assert
  (= (size empty) 0))
(assert (forall ((m Map) (k Key) (v Value))
  (= (size (put m k v))
     (ite (contains m k)
          (size m) ; don't count duplicate puts
          (+ (size m) 1)))))
  
; axioms for contains
(assert (forall ((q Key))
  (= (contains empty q)
     false)))
(assert (forall ((m Map) (q Key) (k Key) (v Value))
  (= (contains (put m k v) q)
     (or (= k q) (contains m q)))))

; axiom for get
(assert (forall ((m Map) (q Key) (k Key) (v Value))
  (= (get (put m k v) q)
     (ite (= k q) v
          (get m q)))))

; axioms for del
(assert (forall ((q Key))
  (= (del empty q)
     empty)))
(assert (forall ((m Map) (q Key) (k Key) (v Value))
  (= (del (put m k v) q)
     (ite (= k q)
          (del m q)
          (put (del m q) k v)))))

; extensionality
(assert (forall ((m1 Map) (m2 Map))
  (=> (forall ((k Key))
        (and (= (contains m1 k) (contains m2 k))
             (=> (contains m1 k) (= (get m1 k) (get m2 k)))))
      (= m1 m2))))
