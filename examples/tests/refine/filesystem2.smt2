;! Cuvee -format -declare-implied -z3

(declare-const empty File)
(declare-const null  Address)

(declare-const undef_f File)
(declare-const undef_a Address)

(assert (distinct empty undef_f))
(assert (distinct null undef_a))

(declare-const fs0 (Array Name File))
(assert (forall ((name Name))
  (= (select fs0 name) undef_f)))

(declare-const index0 (Array Name Address))
(assert (forall ((name Name))
  (= (select index0 name) undef_a)))

(define-class
  AbstractFS
  ((fs (Array Name File)))
  (init () ()
    (assign (fs fs0)))
  (create ((name Name)) ()
     (assign ((select fs name) empty))
     :precondition (= (select fs name) undef_f))
  (delete ((name Name)) ()
     (assign ((select fs name) undef_f))
     :precondition (distinct (select fs name) undef_f))
  (read ((name Name)) ((file File))
     (assign (file (select fs name)))
     :precondition (distinct (select fs name) undef_f)
     :postcondition (distinct file undef_f))
  (write ((name Name) (file File)) ()
     (assign ((select fs name) file))
     :precondition (and (distinct (select fs name) undef_f)
                        (distinct file undef_f))))

(define-class
  FlashFS
  ((index (Array Name Address))
   (disk  (Array Address File)))
  (init () ()
    (assign (index index0)
            ((select disk null) empty)))
  (create ((name Name)) ()
     (assign ((select index name) null))
     :precondition (= (select index name) undef_a))
  (delete ((name Name)) ()
     (assign ((select index name) undef_a))
     :precondition (distinct (select index name) undef_a))
  (read ((name Name)) ((file File))
     (assign (file (select disk (select index name))))
     :precondition (distinct (select index name) undef_a)
     :postcondition (distinct file undef_f))
  (write ((name Name) (file File)) ()
     (local (addr Address))
     (assume (exists ((addr Address)) (and (= (select disk addr) undef_f)
                                           (distinct addr undef_a))))
     (choose (addr) (and (= (select disk addr) undef_f) (distinct addr undef_a)))
     (assign ((select disk addr) file))
     (assign ((select index name) addr))
     :precondition (and (distinct (select index name) undef_a)
                        (distinct file undef_f))))

(push)
  (define-fun R
    ((fs (Array Name File))
     (index (Array Name Address))
     (disk (Array Address File)))
    Bool
    ; disk[null] = empty abduzieren!
    (and (= (select disk null) empty)
         (forall ((name Name)) (and
                 (=> (distinct (select fs name) undef_f)
                     (and (distinct (select index name) undef_a)
                          (= (select fs name)
                             (select disk (select index name)))))
                 (=> (= (select fs name) undef_f)
                     (= (select index name) undef_a))))))
  (set-info :status sat)
  ; prove that refinement relation is well-defined
  (check-sat)

  (verify-refinement AbstractFS FlashFS R)
  (set-info :status unsat)
  (check-sat)
(pop)

; we can't do that yet :(
;(push)
;  (verify-refinement AbstractFS FlashFS R :synthesize output precondition)
;
;  (set-info :status unsat)
;  (check-sat)
;(pop)