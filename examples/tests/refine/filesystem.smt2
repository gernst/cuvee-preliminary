;! Cuvee -z3

(declare-sort Name)
(declare-sort File)
(declare-sort Address)

(declare-const none  Name)
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
    :precondition (distinct name none))
  (write ((name Name) (file File))
         ()
    (assign (fs (store fs name file)))
    :precondition (distinct name none)))

(define-class
  FlashFS
  ((index (Array Name Address))
   (disk  (Array Address File)))
  (init () ()
    (assign (index index0)))
  (read ((name Name))
        ((file File))
    (assign (file (select disk (select index name))))
    :precondition (distinct name none))
  (write ((name Name) (file File))
         ()
    (local  (addr Address))
    (choose (addr)
      (and (distinct addr null)
           (= (select disk addr) empty)))
    (assign (index (store index name addr))
            (disk  (store disk  addr file)))
    :precondition (distinct name none)))

(declare-fun R 
  ((Array Name File)
   (Array Name Address)
   (Array Address File))
   Bool)

(assert (forall 
  ((fs    (Array Name File))
   (index (Array Name Address))
   (disk  (Array Address File)))
  (= (R fs index disk)
     (forall ((name Name))
       (=> (distinct name none)
           (= (select fs name)
              (select disk (select index name))))))))

(verify-refinement
  AbstractFS FlashFS R)
(check-sat)
