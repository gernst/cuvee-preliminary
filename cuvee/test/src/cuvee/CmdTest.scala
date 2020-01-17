package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object CmdTest extends TestSuite {
  test("cannot declare duplicate procedure parameters") {
    try {
      DefineProc(Id("my-proc"), List(Formal(Id("x"), Sort("Int")), Formal(Id("x"), Sort("Int"))), List(), Block(List()), True, True).check
      fail("should have thrown an error")
    } catch {
      case e: Error => assertEquals(e.getMessage, "The method my-proc declares duplicate input parameters x")
    }
  }

  test("simple refinement verification condition") {
    val ac = DefineClass("abstract-counter", List(("counter", "Int")), List(
      DefineProc("init", List(), List(), "counter" := 0, True, True),
      DefineProc("increment", List(), List(), "counter" := (Id("counter") + 1), True, True),
      DefineProc("get-count", List(), List(("count", "Int")), "count" := "counter", True, True)
    ))

    val cc = DefineClass("concrete-counter", List(("counter", "Int")), List(
      DefineProc("init", List(), List(), "counter" := 0, True, True),
      DefineProc("increment", List(), List(), "counter" := ("counter" - 1), True, True),
      DefineProc("get-count", List(), List(("count", "Int")), "count" := (Num(0) - "counter"), True, True)
    ))

    val rel = DefineRefinement(("as", "abstract-counter"), ("cs", "concrete-counter"), "as_counter" === ((Num(0) - "cs_counter")))
    val st = State.default define ac define cc
    val vcs = rel verificationConditions st

    // init
    assertEquals(vcs head, (And(List()) && And(List())) ==> (Num(0) === Num(0) - 0))

    // increment
    assertEquals(vcs(1), Forall(List(("as_counter", "Int"), ("cs_counter", "Int")),
      ("as_counter" === Num(0) - "cs_counter") // rel
        ==> (True // aPre
        ==> (True // cPre
        && (True // insEq
        ==> (True // outsEq
        && (Id("as_counter") + 1 === Num(0) - ("cs_counter" - 1)))))))) // rel

    // get-count
    assertEquals(vcs(2), Forall(List(("as_counter", "Int"), ("cs_counter", "Int")),
      ("as_counter" === Num(0) - "cs_counter") // rel
        ==> (True // aPre
        ==> (True // cPre
        && (True // insEq
        ==> (("as_counter" === Num(0) - "cs_counter") // outsEq
        && (Id("as_counter") === Num(0) - "cs_counter"))))))) // rel
  }

  test("refinement where inlining of post-relation class members does not work") {
    Expr._index = 0 // reset since we're testing against a specific index

    val ac = DefineClass("abstract-counter", List(("counter", "Int")), List(
      DefineProc("init", List(), List(), "counter" := 0, True, True),
      DefineProc("increment", List(), List(), "counter" := (Id("counter") + 1), True, True)
    ))

    val cc = DefineClass("concrete-counter", List(("counter", "Int")), List(
      DefineProc("init", List(), List(), "counter" := 0, True, True),
      DefineProc("increment", List(), List(), Spec(List("counter"), True, "counter" === (Old("counter") + 1)), True, True)
    ))

    val rel = DefineRefinement(("as", "abstract-counter"), ("cs", "concrete-counter"), "as_counter" === ((Num(0) - "cs_counter")))
    val st = State.default define ac define cc
    val vcs = rel verificationConditions st

    assertEquals(vcs(1), Forall(List(("as_counter", "Int"), ("cs_counter", "Int")),
      ("as_counter" === Num(0) - "cs_counter") // rel
        ==> (True // aPre
        ==> (True // cPre
        && (True // insEq
        ==> (True // outsEq
        && Forall(List(("cs_counter1", "Int")), ("cs_counter1" === Id("cs_counter") + 1) ==> (True && Id("as_counter") + 1 === Num(0) - "cs_counter1")))))))) // rel
  }

  test("refinement for account") {
    val ac = DefineClass("simple-account", List(("balance", "Int")), List(
      DefineProc("init", List(), List(), "balance" := 0, True, True),
      DefineProc("deposit", List(("amount", "Int")), List(("new-balance", "Int")), Block(List("balance" := (Id("balance") + "amount"), "new-balance" := "balance")), Id("amount") > 0, True),
      DefineProc("withdraw", List(("amount", "Int")), List(("new-balance", "Int")), Block(List("balance" := (Id("balance") - "amount"), "new-balance" := "balance")), Id("amount") > 0 && Id("amount") <= "balance", True))
    )

    val cc = DefineClass("double-account", List(("debit", "Int"), ("credit", "Int"), ("max-overdraft", "Int")), List(
      DefineProc("init", List(("maximum-overdraft", "Int")), List(), Block(List("debit" := 0, "credit" := 0, "max-overdraft" := "maximum-overdraft")), Id("maximum-overdraft") >= 0, True),
      DefineProc("deposit", List(("amount", "Int")), List(("new-balance", "Int")), Block(List("credit" := (Id("credit") + "amount"), "new-balance" := Id("credit") - "debit")), Id("amount") > 0, True),
      DefineProc("withdraw", List(("amount", "Int")), List(("new-balance", "Int")), Block(List("debit" := (Id("debit") + "amount"), "new-balance" := Id("credit") - "debit")), Id("amount") > 0 && Id("amount") <= Id("credit") - "debit" + "max-overdraft", True))
    )

    val rel = DefineRefinement(("as", "simple-account"), ("cs", "double-account"), "as_balance" === Id("cs_credit") - "cs_debit")
    val st = State.default define ac define cc
    val vcs = rel verificationConditions st

    vcs.foreach(vc => println(PrettyPrinter.printExpr(vc)))
  }
}
