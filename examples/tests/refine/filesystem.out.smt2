(declare-sort Name 0)
(declare-sort File 0)
(declare-sort Address 0)
(declare-fun empty () File)
(declare-fun null () Address)
(declare-fun fs0 () (Array Name File))
(declare-fun index0 () (Array Name Address))
(declare-fun R ((Array Name File) (Array Name Address) (Array Address File)) Bool)
(push 1)
(assert (forall ((name Name)) (= (select fs0 name) empty)))
(assert (forall ((name Name)) (= (select index0 name) null)))
(assert (forall ((fs (Array Name File)) (index (Array Name Address)) (disk (Array Address File))) (= (R fs index disk) (forall ((name Name)) (=> (distinct (select fs name) empty) (and (distinct (select index name) null) (= (select fs name) (select disk (select index name)))))))))
(assert (not (and (forall ((fs (Array Name File)) (index (Array Name Address)) (disk (Array Address File))) (=> (and true true) (R fs0 index0 (store disk null empty)))) (forall ((fs (Array Name File)) (name Name) (file File) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name) (|file'| File)) (=> (= name |name'|) (=> (and (distinct (select fs name) empty) (R fs index disk)) (and (distinct (select index |name'|) null) (and (= (select fs name) (select disk (select index |name'|))) (R fs index disk)))))) (forall ((fs (Array Name File)) (name Name) (file File) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name) (|file'| File)) (=> (and (= name |name'|) (= file |file'|)) (=> (and (distinct (select fs name) empty) (R fs index disk)) (and (distinct (select index |name'|) null) (forall ((addr Address)) (and true (=> (exists ((addr Address)) (and (distinct addr null) (= (select disk addr) empty))) (and (exists ((addr1 Address)) (and (distinct addr1 null) (= (select disk addr1) empty))) (forall ((addr1 Address)) (=> (and (distinct addr1 null) (= (select disk addr1) empty)) (and true (R (store fs name file) (store index |name'| addr1) (store disk addr1 |file'|))))))))))))))))
(check-sat)
(pop 1)
