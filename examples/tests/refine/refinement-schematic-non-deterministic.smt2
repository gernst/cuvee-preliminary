;! Cuvee -format -z3

; The abstract refinement schema between an abstract and a concrete class
; where the procedures are non-deterministic. The functions Aop and Cop describe the relation
; between the input of, state before, state after, and output of the procedure.

; "as" (abstract state) is a reserved keyword and Z3 doesn't allow it even if we escape it.
; we use "a" instead of "as" and "c" instead of "cs" for consistency

(declare-sort As)
(declare-sort Cs)
(declare-fun Ainit (As) Bool)
(declare-sort In)
(declare-sort Out)
(declare-fun Aop (In As As Out) Bool)
(declare-fun Apre (As In) Bool)
(declare-fun Cinit (Cs) Bool)
(declare-fun Cop (In Cs Cs Out) Bool)
(declare-fun Cpre (Cs In) Bool)
(declare-fun R (As Cs) Bool)

(declare-const a0 As)
(declare-const c0 Cs)

(define-class A ((a As))
  (init () () (choose (a) (Ainit a)))
  (op ((in In)) ((out Out))
    (choose (a out) (Aop in (old a) a out))
    :precondition (Apre a in)))

(define-class C ((c Cs))
  (init () () (choose (c) (Cinit c)))
  (op ((in In)) ((out Out))
    (choose (c out) (Cop in (old c) c out))
    :precondition (Cpre c in)))

(assert (not
  (= (refines A C R)
     (forall ((a As) (in In) (c Cs) (in In))
       (and (exists ((c Cs)) (Cinit c))
            (forall ((c Cs))
              (=> (Cinit c)
                  (exists ((a As)) (and (Ainit a) (R a c)))))
            (forall ((a As) (in In) (c Cs))
              (=> (and (Apre a in) (R a c))
                  (and (Cpre c in)
                       (exists ((|c'| Cs) (out Out))
                               (Cop in c |c'| out))
                       (forall ((|c'| Cs) (out Out))
                               (=> (Cop in c |c'| out)
                                   (exists ((|a'| As) (|out'| Out))
                                           (and (Aop in a |a'| out)
                                                (= out |out'|)
                                                (R |a'| |c'|)))))))))))))

(set-info :status unsat)
(check-sat)