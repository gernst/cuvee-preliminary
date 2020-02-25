(set-logic ALL)
(push 1)
(assert (not (and
    ; init
    (forall ((b Int) (d Int) (c Int) (l Int) (|od-lim'| Int)) (=> (and true (>= |od-lim'| 0)) (and (= 0 (- 0 0)) (>= |od-lim'| 0))))

    ; deposit
    (forall ((b Int) (amount Int) (new-balance Int) (d Int) (c Int) (l Int) (|add'| Int) (|increased'| Int))
        (=> (= amount |add'|) (=> (and (> amount 0) (= b (- c d)) (>= l 0)) (and (> amount 0)
            ; post
            (and (= (+ b amount) (- (+ c |add'|) d)) (= (+ b amount) (- (+ c |add'|) d)) (>= l 0))))))

    ; withdraw
    (forall ((b Int) (amount Int) (new-balance Int) (d Int) (c Int) (l Int) (|remove'| Int) (|decreased'| Int))
        (=> (= amount |remove'|) (=> (and (> amount 0) (<= amount b) (= b (- c d)) (>= l 0)) (and (> amount 0)
            ; post
            (<= amount (+ (- c d) l)) (and (= (- b amount) (- c (+ d |remove'|))) (= (- b amount) (- c (+ d |remove'|))) (>= l 0)))))))))
(check-sat)
(pop 1)
