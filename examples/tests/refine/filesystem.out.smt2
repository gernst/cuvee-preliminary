(declare-sort File 0)
(declare-fun empty () File)
(declare-sort Address 0)
(declare-fun null () Address)
(declare-sort Name 0)
(declare-fun fs0 () (Array Name File))
(declare-fun index0 () (Array Name Address))
(push 1)
(declare-fun R ((Array Name File) (Array Name Address) (Array Address File)) Bool)
(push 1)
(assert
  (forall
    ((name Name))
    (=
      (select fs0 name)
      empty)))
(assert
  (forall
    ((name Name))
    (=
      (select index0 name)
      null)))
(assert
  (forall
    ((fs (Array Name File)) (index (Array Name Address)) (disk (Array Address File)))
    (=
      (R fs index disk)
      (forall
        ((name Name))
          (and
             (=> (distinct (select fs name) empty)
                 (and (distinct (select index name) null)
                    (= (select fs name)
                    (select disk (select index name)))))
             (=> (= (select fs name) empty)
                 (= (select index name) null)))))))
(assert
  (not
    (and
      (forall
        ((disk (Array Address File)))
        (=>
          (and
            true
            true
            true)
          (and
            true
            true
            (R fs0 index0 (store disk null empty)))))
      (forall
        ((fs (Array Name File)) (name Name) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name))
        (=>
          (and
            (= |name'| name)
            (distinct (select fs name) empty)
            (R fs index disk))
          (and
            (distinct (select index |name'|) null)
            (= (select disk (select index |name'|)) (select fs name))
            (R fs index disk))))
      (forall
        ((fs (Array Name File)) (name Name) (file File) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name) (|file'| File))
        (=>
          (and
            (= |name'| name)
            (= |file'| file)
            (= (select fs name) empty)
            (distinct file empty)
            (R fs index disk))
          (and
            (= (select index |name'|) null)
            (distinct |file'| empty)
            true
            (=>
              (exists
                ((addr Address))
                (and
                  (distinct addr null)
                  (= (select disk addr) empty)))
              (and
                (exists
                  ((addr1 Address))
                  (and
                    (distinct addr1 null)
                    (= (select disk addr1) empty)))
                (forall
                  ((addr1 Address))
                  (=>
                    (and
                      (distinct addr1 null)
                      (= (select disk addr1) empty))
                    (and
                      true
                      (R (store fs name file) (store index |name'| addr1) (store disk addr1 |file'|)))))))))))))
(check-sat)
(pop 1)
(pop 1)
(push 1)
(define-fun R ((fs (Array Name File)) (index (Array Name Address)) (disk (Array Address File))) Bool
  (and
    (forall
      ((name Name))
      (or
        (= (select fs name) empty)
        (= (select fs name) (select disk (select index name)))))
    (forall
      ((name Name))
      (or
        (= (select fs name) empty)
        (distinct (select index name) null)))
    (forall
      ((name Name) (file File))
      (or
        (not
          (= (select fs name) empty))
        (= file empty)
        (and
          (= (select index name) null)
          (distinct file empty))))))
(push 1)
(assert
  (forall
    ((name Name))
    (=
      (select fs0 name)
      empty)))
(assert
  (forall
    ((name Name))
    (=
      (select index0 name)
      null)))
(assert
  (not
    (and
      (forall
        ((disk (Array Address File)))
        (=>
          (and
            true
            true
            true)
          (and
            true
            true
            (R fs0 index0 (store disk null empty)))))
      (forall
        ((fs (Array Name File)) (name Name) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name))
        (=>
          (and
            (= |name'| name)
            (distinct (select fs name) empty)
            (R fs index disk))
          (and
            (distinct (select index |name'|) null)
            (= (select disk (select index |name'|)) (select fs name))
            (R fs index disk))))
      (forall
        ((fs (Array Name File)) (name Name) (file File) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name) (|file'| File))
        (=>
          (and
            (= |name'| name)
            (= |file'| file)
            (= (select fs name) empty)
            (distinct file empty)
            (R fs index disk))
          (and
            (= (select index |name'|) null)
            (distinct |file'| empty)
            true
            (=>
              (exists
                ((addr Address))
                (and
                  (distinct addr null)
                  (= (select disk addr) empty)))
              (and
                (exists
                  ((addr3 Address))
                  (and
                    (distinct addr3 null)
                    (= (select disk addr3) empty)))
                (forall
                  ((addr3 Address))
                  (=>
                    (and
                      (distinct addr3 null)
                      (= (select disk addr3) empty))
                    (and
                      true
                      (R (store fs name file) (store index |name'| addr3) (store disk addr3 |file'|)))))))))))))
(check-sat)
(pop 1)
(pop 1)
