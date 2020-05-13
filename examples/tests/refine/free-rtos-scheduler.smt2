;! Cuvee -format -z3

(set-option :declare-implied true)

(declare-datatypes ((State 0))
  (((nonexistent) (ready) (isblocked) (suspended) (running))))

;special values
(declare-const idle Task)
(declare-const bare Context)

;initial values
(declare-const state0 (Array Task State))
(assert (forall ((t Task)) (=> (distinct t idle) (= (select state0 t) nonexistent))))
(assert (forall ((t Task)) (=> (= t idle) (= (select state0 t) running))))

(declare-const ctx0 (Array Task Context))
(assert (forall ((t Task)) (= (select ctx0 t) bare)))

(declare-const prio0 (Array Task Int))
(assert (forall ((t Task)) (= (select prio0 t) 0)))

(declare-const tasks0 (Array Task Bool))
(assert (forall ((t Task)) (= (select tasks0 t) (= t idle))))

(define-fun inv
  ((cur Task)
   (phys Context)
   (state (Array Task State))
   (ctx (Array Task Context))
   (prio (Array Task Int))
   (tasks (Array Task Bool)))
  Bool
  (and (or (= (select state idle) running) (= (select state idle) ready))
       (= (select prio idle) 0)
       (forall ((t Task)) (= (= (select state t) nonexistent) (not (select tasks t))))
       (forall ((t Task)) (= (= (select state t) running) (= t cur)))
       (forall ((t Task)) (=> (= (select state t) ready) (<= (select prio t) (select prio cur))))
       ))

(define-class Impl
  ((cur Task)
   (phys Context)
   (state (Array Task State))
   (ctx (Array Task Context))
   (prio (Array Task Int))
   (tasks (Array Task Bool)))

  (init () ()
    (assign (cur idle)
            (phys bare)
            (state state0)
            (ctx ctx0)
            (prio prio0)
            (tasks tasks0))
    :postcondition (inv cur phys state ctx prio tasks))

  ; "private" procedure. No postcondition => inlined
  (reschedule ((t Task) (st State)) ()
    (assign ((select state cur) st))
    (assign ((select state t) running))
    (assign ((select ctx cur) phys))
    (assign (phys (select ctx t)))
    (assign (cur t) (t cur)))

  (task-create ((t Task) (p Int)) ()
    (assign ((select tasks t) true)
            ((select state t) ready)
            ((select prio t) p))
    (if (< (select prio cur) p) (block
      (call reschedule (t ready) ())
    ))
    :precondition (and (inv cur phys state ctx prio tasks)
                       (not (select tasks t)))
    :postcondition (inv cur phys state ctx prio tasks))

  (task-delete ((t Task)) ()
    (local (|t'| Task))
    (assign ((select tasks t) false)
            ((select state t) nonexistent)
            ((select ctx t) bare))
    (if (= cur t) (block
      (assume (exists ((|t'| Task)) (and (= (select state |t'|) ready)
                                    (forall ((t0 Task)) (=> (= (select state t0) ready)
                                                            (<= (select prio t0) (select prio |t'|)))))))
      (choose (|t'|) (and (= (select state |t'|) ready)
                        (forall ((t0 Task)) (=> (= (select state t0) ready)
                                                (<= (select prio t0) (select prio |t'|))))))
      (call reschedule (|t'| nonexistent) ())
    ))
    :precondition (and (inv cur phys state ctx prio tasks)
                       (distinct t idle)
                       (select tasks t))
    :postcondition (inv cur phys state ctx prio tasks))

  (task-suspend ((t Task)) ()
    (local (|t'| Task))
    (assign ((select state t) suspended))
    (if (= cur t) (block
      (assume (exists ((|t'| Task)) (and (= (select state |t'|) ready)
                                    (forall ((t0 Task)) (=> (= (select state t0) ready)
                                                            (<= (select prio t0) (select prio |t'|)))))))
      (choose (|t'|) (and (= (select state |t'|) ready)
                        (forall ((t0 Task)) (=> (= (select state t0) ready)
                                                (<= (select prio t0) (select prio |t'|))))))
      (call reschedule (|t'| suspended) ())
    ))
    :precondition (and (inv cur phys state ctx prio tasks)
                       (distinct t idle)
                       (select tasks t)
                       (or (= (select state t) ready)
                           (= (select state t) isblocked)
                           (= (select state t) running)
                           (= (select state t) suspended)))
    :postcondition (inv cur phys state ctx prio tasks))

  (task-resume ((t Task)) ()
    (assign ((select state t) ready))
    (if (< (select prio cur) (select prio t)) (block
      (call reschedule (t ready) ())
    ))
    :precondition (and (inv cur phys state ctx prio tasks)
                       (distinct t idle)
                       (select tasks t)
                       (= (select state t) suspended))
    :postcondition (inv cur phys state ctx prio tasks))

  (task-set-prio ((t Task) (p Int)) ()
    (local (|t'| Task))
    (assign ((select prio t) p))
    (if (and (= cur t)
             (exists ((t0 Task)) (and (= (select state t0) ready)
                                      (< p (select prio t0))))) (block
      (assume (exists ((|t'| Task)) (and (= (select state |t'|) ready)
                                    (forall ((t0 Task)) (=> (= (select state t0) ready)
                                                            (<= (select prio t0) (select prio |t'|)))))))
      (choose (|t'|) (and (= (select state |t'|) ready)
                        (forall ((t0 Task)) (=> (= (select state t0) ready)
                                                (<= (select prio t0) (select prio |t'|))))))
      (call reschedule (|t'| ready) ()))
      ; else
      (if (< (select prio cur) p) (call reschedule (t ready) ())))
    :precondition (and (inv cur phys state ctx prio tasks)
                       (distinct t idle)
                       (select tasks t))
    :postcondition (inv cur phys state ctx prio tasks))
)

(verify-class Impl)
(set-info :status unsat)
(check-sat)