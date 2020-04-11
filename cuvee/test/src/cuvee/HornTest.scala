package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object HornTest extends TestSuite {
  test("motivating example") {
    val expr = e"""(and
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
            (distinct (select fs name) empty)
            (R fs index disk))
          (and
            (distinct (select index |name'|) null)
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
                      (R (store fs name file) (store index |name'| addr1) (store disk addr1 |file'|)))))))))))"""
    Printer.format = true
    Horn.split(expr).foreach(println)
    test(expr, None, s => {
      s.declare("Name", 0)
      s.declare("File", 0)
      s.declare("Address", 0)
      s.declare("empty", "File")
      s.declare("null", "Address")
      s.declare("fs0", Type.array("Name", "File"))
      s.declare("index0", Type.array("Name", "Address"))
      s.declare("R", List(Type.array("Name", "File"), Type.array("Name", "Address"), Type.array("Address", "File")), "Bool")
    })
  }

  test("implication") {
    test(e"(=> a b)", Some(List(Horn(Nil, List(e"a"), e"b"))), bools("a", "b"))
  }

  test("trivial antecedent") {
    test(e"(=> true b)", Some(List(Horn(Nil, Nil, e"b"))), bools("b"))
  }

  test("trivial succedent") {
    test(e"(=> a true)", Some(Nil), bools("a"))
  }

  test("trivial sequent") {
    test(e"(=> a a)", Some(Nil), bools("a"))
  }

  test("antecedent is flattened") {
    test(e"(=> (and a b ) c)", Some(List(Horn(Nil, List(e"a", e"b"), e"c"))), bools("a", "b", "c"))
  }

  test("succedent is flattened") {
    test(e"(=> a (and b c))", Some(List(Horn(Nil, List(e"a"), e"b"), Horn(Nil, List(e"a"), e"c"))),
      bools("a", "b", "c"))
  }

  test("variable substitution") {
    test(e"(forall ((a bool)) (=> (= a b) a))", Some(List(Horn(Nil, Nil, e"b"))), bools("b"))
  }

  test("variable substitution reversed") {
    test(e"(forall ((b bool)) (=> (= a b) b))", Some(List(Horn(Nil, Nil, e"a"))), bools("a"))
  }

  test("fun variable substitution") {
    test(e"(forall ((a bool)) (=> (and (= a b) (distinct a b)) c))", Some(Nil), bools("b", "c"))
  }

  test("global variable substitution") {
    test(e"(=> (= a b) a)", Some(List(Horn(Nil, List(e"(= a b)"), e"b"))), bools("a", "b"))
  }

  test("reverse global variable substitution") {
    test(e"(=> (= (f c) b) b)", Some(List(Horn(Nil, List(e"(= (f c) b)"), e"(f c)"))),
      bools("b", "c").compose(s => { s.declare("f", List("bool"), "bool"); s }))
  }

  test("mixed substitution") {
    test(e"(forall ((a bool)) (=> (and (= (f c) b) (= c d) (= a b) a) (f d)))",
      Some(Nil),
      bools("b", "c", "d").andThen(s => s.declare("f", List("bool"), "bool")))
  }

  test("foralls") {
    test(e"(forall ((a Int)) (and (P a) (forall ((a Int)) (Q a))))",
      Some(List(Horn(List(("a", "Int")), Nil, e"(P a)"), Horn(List(("a", "Int")), Nil, e"(Q a)"))),
      intPredicates("P", "Q"))
  }

  test("exists") {
    test(e"(forall ((a Int)) (=> (and (P a) (exists ((b Int)) (Q b)) (exists ((a Int)) (R a))) (S a)))",
      Some(List(Horn(List(("a", "Int"), ("b", "Int"), ("a1", "Int")), List(e"(P a)", e"(Q b)", App("R", "a1")), e"(S a)"))),
      intPredicates("P", "Q", "R", "S"))
  }

  def bools(names: String *): Sink => Sink = constants(names map((_, "bool")) :_*)

  def constants(decls: (String, String) *): Sink => Sink = s => {
    decls.foreach(d => s.declare(d._1, d._2))
    s
  }

  def intPredicates(names: String *): Sink => Sink = s => {
    names.foreach(n => s.declare(n, List("Int"), "bool"))
    s
  }

  def test[S](initial: Expr, expected: Option[List[Horn]], env: Sink => S): Unit = {
    Expr._index = 0; // reset because we have reference output
    val actual = Horn.split(initial)
    expected.foreach(assertEquals(actual, _))

    val actualExpr = And(actual.map(_.toExpr))
    val allowedFree = initial.free + True + False
    assert(actualExpr.free.subsetOf(allowedFree), "no new free variables")

    val solver = Solver.z3()
    env(solver)
    // prove that expression is equal
    assertEquals(solver.check(actualExpr !== initial), Unsat)
  }
}
