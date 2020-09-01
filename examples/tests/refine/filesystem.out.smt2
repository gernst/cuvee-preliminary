(declare-sort File 0)
(declare-sort Address 0)
(declare-sort Name 0)
(declare-fun empty () File)
(declare-fun null () Address)
(declare-fun fs0 () (Array Name File))
(declare-fun index0 () (Array Name Address))
(push 1)
(declare-fun R ((Array Name File) (Array Name Address) (Array Address File)) Bool)
(push 1)
(assert (forall ((name Name)) (= (select fs0 name) empty)))
(assert (forall ((name Name)) (= (select index0 name) null)))
(assert (forall ((fs (Array Name File)) (index (Array Name Address)) (disk (Array Address File))) (= (R fs index disk) (forall ((name Name)) (and (=> (distinct (select fs name) empty) (and (distinct (select index name) null) (= (select fs name) (select disk (select index name))))) (=> (= (select fs name) empty) (= (select index name) null)))))))
(assert (not (and (and (forall ((disk (Array Address File))) (=> (and (and true true) true) (and true (and true (R fs0 index0 (store disk null empty)))))) (forall ((fs (Array Name File)) (name Name) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name)) (=> (and (and (= |name'| name) (distinct (select fs name) empty)) (R fs index disk)) (and (distinct (select index |name'|) null) (and (= (select disk (select index |name'|)) (select fs name)) (R fs index disk)))))) (forall ((fs (Array Name File)) (name Name) (file File) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name) (|file'| File)) (=> (and (and (and (= |name'| name) (= |file'| file)) (and (= (select fs name) empty) (distinct file empty))) (R fs index disk)) (and (and (= (select index |name'|) null) (distinct |file'| empty)) (and true (=> (exists ((addr1 Address)) (and (distinct addr1 null) (= (select disk addr1) empty))) (and (exists ((addr2 Address)) (and (distinct addr2 null) (= (select disk addr2) empty))) (forall ((addr2 Address)) (=> (and (distinct addr2 null) (= (select disk addr2) empty)) (and true (R (store fs name file) (store index |name'| addr2) (store disk addr2 |file'|))))))))))))))
(check-sat)
(pop 1)
(pop 1)
(push 1)
(define-fun R ((fs (Array Name File)) (index (Array Name Address)) (disk (Array Address File))) Bool (and (and (forall ((name Name)) (or (= (select fs name) empty) (= (select fs name) (select disk (select index name))))) (forall ((name Name)) (or (= (select fs name) empty) (distinct (select index name) null)))) (forall ((name Name) (file File)) (or (or (not (= (select fs name) empty)) (= file empty)) (= (select index name) null)))))
(push 1)
(assert (forall ((name Name)) (= (select fs0 name) empty)))
(assert (forall ((name Name)) (= (select index0 name) null)))
(assert (not (and (and (forall ((disk (Array Address File))) (=> (and (and true true) true) (and true (and true (R fs0 index0 (store disk null empty)))))) (forall ((fs (Array Name File)) (name Name) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name)) (=> (and (and (= |name'| name) (distinct (select fs name) empty)) (R fs index disk)) (and (distinct (select index |name'|) null) (and (= (select disk (select index |name'|)) (select fs name)) (R fs index disk)))))) (forall ((fs (Array Name File)) (name Name) (file File) (index (Array Name Address)) (disk (Array Address File)) (|name'| Name) (|file'| File)) (=> (and (and (and (= |name'| name) (= |file'| file)) (and (= (select fs name) empty) (distinct file empty))) (R fs index disk)) (and (and (= (select index |name'|) null) (distinct |file'| empty)) (and true (=> (exists ((addr5 Address)) (and (distinct addr5 null) (= (select disk addr5) empty))) (and (exists ((addr6 Address)) (and (distinct addr6 null) (= (select disk addr6) empty))) (forall ((addr6 Address)) (=> (and (distinct addr6 null) (= (select disk addr6) empty)) (and true (R (store fs name file) (store index |name'| addr6) (store disk addr6 |file'|))))))))))))))
(check-sat)
(pop 1)
(pop 1)
