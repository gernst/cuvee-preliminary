;! Cuvee -format -declare-implied -z3

; The abstract refinement schema between an abstract and a concrete class
; where the procedures have a functional dependency between input and output.

; "as" (abstract state) is a reserved keyword and Z3 doesn't allow it even if we escape it.
; we use "a" instead of "as" and "c" instead of "cs" for consistency

(declare-const a0 As)
(declare-const c0 Cs)

(define-class
  A
  ((a As))
  (init () ()
    (assign (a a0))
    :postcondition (Ainitpost a))
  (inonly ((in In)) ()
    (assign (a (Ainonly a in)))
    :precondition (Ainonlypre a in)
    :postcondition (Ainonlypost a))
  (through ((in In)) ((out Out))
    (assign (a (Athrougha a in)) (out (Athroughout a in)))
    :precondition (Athroughpre a in)
    :postcondition (Athroughpost a out)))

(define-class
  C
  ((c Cs))
  (init () ()
    (assign (c c0))
    :postcondition (Cinitpost c))
  (inonly ((in In)) ()
    (assign (c (Cinonly c in)))
    :precondition (Cinonlypre c in)
    :postcondition (Cinonlypost c))
  (through ((in In)) ((out Out))
    (assign (c (Cthroughc c in)) (out (Cthroughout c in)))
    :precondition (Cthroughpre c in)
    :postcondition (Cthroughpost c out)))

(assert (not (= (refines A C R)
  (forall ((a As) (in In) (c Cs) (in In)) (and
    (R a0 c0)
    (=> (R a c) (and
      (=> (Ainonlypre a in)
        (and
          (Cinonlypre c in)
          (R (Ainonly a in) (Cinonly c in))))
      (=> (Athroughpre a in)
        (and
          (Cthroughpre c in)
          (= (Athroughout a in) (Cthroughout c in))
          (R (Athrougha a in) (Cthroughc c in)))))))))))

(set-info :status unsat)
(check-sat)