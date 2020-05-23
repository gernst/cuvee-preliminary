;! Cuvee -format -z3

; This example is based on:
; Shu Cheng, Jim Woodcock, and Deepak D’Souza. Using formal reasoning
; on a model of tasks for FreeRTOS. Formal Aspects of Computing, 27(1):167–192, 2015.
;
; https://doi.org/10.1007/s00165-014-0308-9
; https://www-users.cs.york.ac.uk/~jw524/papers/ChengWD15.pdf

(declare-sort Context)
(declare-sort Task)

;special values
(declare-const bare_context Context)
(declare-const idle Task)

(declare-datatypes ((State 0))
  (((nonexistent) (ready) (blocked) (suspended) (running))))

(define-fun transition
  ((l State) (r State))
  Bool
  (and
    (=> (= l blocked) (or (= r nonexistent) (= r ready) (= r running) (= r suspended)))
    (=> (= l nonexistent) (or (= r ready) (= r running)))
    (=> (= l ready) (or (= r nonexistent) (= r running) (= r suspended)))
    (=> (= l running) (or (= r blocked) (= r nonexistent) (= r ready) (= r suspended)))
    (=> (= l suspended) (or (= r nonexistent) (= r ready) (= r running)))))

(define-fun transition_inv
  ((state (Array Task State))
   (|state'| (Array Task State)))
  Bool
  (forall ((st Task)) (=>
    (distinct (select state st) (select |state'| st))
    (transition (select state st) (select |state'| st)))))

(define-fun inv
  ((tasks (Array Task Bool))
   (running_task Task)
   (state (Array Task State))
   (phys_context Context)
   (log_context (Array Task Context))
   (priority (Array Task Int)))
  Bool
  (and (select tasks running_task)
       (select tasks idle)
       (or (= (select state idle) running) (= (select state idle) ready))
       (= (select priority idle) 0)
       (forall ((t Task)) (= (select tasks t) (distinct (select state t) nonexistent)))
       (forall ((t Task)) (= (= (select state t) running) (= t running_task)))
       (forall ((pt Task)) (=> (= (select state pt) ready) (>= (select priority running_task) (select priority pt))))))

;initial values
(declare-const tasks0 (Array Task Bool))
(assert (forall ((t Task)) (= (select tasks0 t) (= t idle))))

(declare-const state0 (Array Task State))
(assert (forall ((t Task)) (= (select state0 t) (ite (= t idle) running nonexistent))))

(declare-const log_context0 (Array Task Context))
(assert (forall ((t Task)) (= (select log_context0 t) bare_context)))

(declare-const priority0 (Array Task Int))
(assert (forall ((t Task)) (= (select priority0 t) 0)))

(define-class Impl
  ((tasks (Array Task Bool))
   (running_task Task)
   (state (Array Task State))
   (phys_context Context)
   (log_context (Array Task Context))
   (priority (Array Task Int)))

  (init () ()
    (assign (tasks tasks0)
            (running_task idle)
            (state state0)
            (phys_context bare_context)
            (log_context log_context0)
            (priority priority0))
    :postcondition (inv tasks running_task state phys_context log_context priority))

  ; "private" procedure. No postcondition => inlined
  (reschedule ((target Task) (st State)) ()
    (assign ((select state target) running))
    (assign ((select state running_task) st))
    (assign ((select log_context running_task) phys_context))
    (assign (phys_context (select log_context target)))
    (assign (running_task target) (target running_task)))

  (create_task ((target Task) (newpri Int)) ()
    (assign ((select tasks target) true)
            ((select state target) ready)
            ((select priority target) newpri))
    (if (< (select priority running_task) newpri) (block
      (call reschedule (target ready) ())
    ))
    :precondition (and (inv tasks running_task state phys_context log_context priority)
                       (= (select state target) nonexistent))
    :postcondition (and (inv tasks running_task state phys_context log_context priority)
                        (transition_inv (old state) state)))

  (delete_task ((target Task)) ()
    (local (topReady Task))
    (assign ((select tasks target) false)
            ((select state target) nonexistent)
            ((select log_context target) bare_context))
    (if (= running_task target) (block
      ; TODO create choose_topReady procedure, but currently bugs out
      (assume (exists ((topReady Task)) (and (= (select state topReady) ready)
                                             (forall ((t Task)) (=> (= (select state t) ready)
                                                                    (>= (select priority topReady) (select priority t)))))))
      (choose (topReady) (and (= (select state topReady) ready)
                              (forall ((t Task)) (=> (= (select state t) ready)
                                                     (>= (select priority topReady) (select priority t))))))
      (call reschedule (topReady nonexistent) ())
    ))
    :precondition (and (inv tasks running_task state phys_context log_context priority)
                       (select tasks target)
                       (distinct target idle))
    :postcondition (and (inv tasks running_task state phys_context log_context priority)
                        (transition_inv (old state) state)))

  (suspend_task ((target Task)) ()
    (local (topReady Task))
    (assign ((select state target) suspended))
    (if (= running_task target) (block
      (assume (exists ((topReady Task)) (and (= (select state topReady) ready)
                                             (forall ((t Task)) (=> (= (select state t) ready)
                                                                    (>= (select priority topReady) (select priority t)))))))
      (choose (topReady) (and (= (select state topReady) ready)
                        (forall ((t Task)) (=> (= (select state t) ready)
                                               (>= (select priority topReady) (select priority t))))))
      (call reschedule (topReady suspended) ())
    ))
    :precondition (and (inv tasks running_task state phys_context log_context priority)
                       (distinct target idle)
                       (select tasks target))
    :postcondition (and (inv tasks running_task state phys_context log_context priority)
                        (transition_inv (old state) state)))

  (resume_task ((target Task)) ()
    (assign ((select state target) ready))
    (if (< (select priority running_task) (select priority target)) (block
      (call reschedule (target ready) ())
    ))
    :precondition (and (inv tasks running_task state phys_context log_context priority)
                       (= (select state target) suspended))
    :postcondition (and (inv tasks running_task state phys_context log_context priority)
                        (transition_inv (old state) state)))

  (change_task_priority ((target Task) (newpri Int)) ()
    (local (topReady Task))
    (assign ((select priority target) newpri))
    (if (and (= running_task target)
             (exists ((t Task)) (and (= (select state t) ready)
                                     (< newpri (select priority t))))) (block
      (assume (exists ((topReady Task)) (and (= (select state topReady) ready)
                                    (forall ((t Task)) (=> (= (select state t) ready)
                                                           (>= (select priority topReady) (select priority t)))))))
      (choose (topReady) (and (= (select state topReady) ready)
                         (forall ((t Task)) (=> (= (select state t) ready)
                                                (>= (select priority topReady) (select priority t))))))
      (call reschedule (topReady ready) ()))
      ; else
      (if (< (select priority running_task) newpri) (call reschedule (target ready) ())))
    :precondition (and (inv tasks running_task state phys_context log_context priority)
                       (distinct target idle)
                       (select tasks target))
    :postcondition (and (inv tasks running_task state phys_context log_context priority)
                        (transition_inv (old state) state)))
)

(verify-class Impl)
(set-info :status unsat)
(check-sat)