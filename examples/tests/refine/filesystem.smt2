;! Cuvee -z3

(declare-sort Name)
(declare-sort File)
(declare-sort Address)

(declare-const empty File)
(declare-const null  Address)

(declare-const fs0 (Array Name File))
(assert (forall ((name Name))
  (= (select fs0 name)
     empty)))

(declare-const index0 (Array Name Address))
(assert (forall ((name Name))
  (= (select index0 name)
     null)))

(define-class
  AbstractFS
  ((fs (Array Name File)))
  (init () ()
    (assign (fs fs0)))
  (read ((name Name))
        ((file File))
    (assign (file (select fs name)))
    :precondition (distinct (select fs name) empty))
  (write ((name Name) (file File))
         ()
    (assign ((select fs name) file))
    :precondition (distinct (select fs name) empty)))

(define-class
  FlashFS
  ((index (Array Name Address))
   (disk  (Array Address File)))
  (init () ()
    (assign (index index0)
            ((select disk null) empty)))
  (read ((name Name))
        ((file File))
    (assign (file (select disk (select index name))))
    :precondition (distinct (select index name) null))
  (write ((name Name) (file File))
         ()
    (local  (addr Address))
    (assume (exists ((addr Address))
      (and (distinct addr null)
           (= (select disk addr) empty))))
    (choose (addr)
      (and (distinct addr null)
           (= (select disk addr) empty)))
    (assign ((select index name) addr)
            ((select disk  addr) file))
    :precondition (distinct (select index name) null)))

(declare-fun R 
  ((Array Name File)
   (Array Name Address)
   (Array Address File))
   Bool)

(push)
  (assert (forall 
    ((fs    (Array Name File))
     (index (Array Name Address))
     (disk  (Array Address File)))
    (= (R fs index disk)
       (forall ((name Name))
         (=> (distinct (select fs name) empty)
             (and (distinct (select index name) null)
                  (= (select fs name)
                     (select disk (select index name)))))))))

  (verify-refinement AbstractFS FlashFS R)
  (set-info :status unsat)
  (check-sat)
(pop)

(push)
  (verify-refinement AbstractFS FlashFS R :synthesize output)
  (set-info :status unsat)
  (check-sat)
(pop)

