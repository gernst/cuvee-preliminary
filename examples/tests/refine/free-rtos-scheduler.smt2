;!Cuvee -format -z3

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
       (forall ((t Task)) (=> (= (select state t) ready) (<= (select prio t) (select prio cur))))))

(define-class Impl
  ((cur Task)
   (phys Context)
   (state (Array Task State))
   (ctx (Array Task Context))
   (prio (Array Task Int))
   (tasks (Array Task Bool)))
  (init () ()
    (assign (cur idle))
    (assign (phys bare))
    (assign (state state0))
    (assign (ctx ctx0))
    (assign (prio prio0))
    (assign (tasks tasks0))
    :postcondition (inv cur phys state ctx prio tasks))
)

(verify-class Impl)
(check-sat)