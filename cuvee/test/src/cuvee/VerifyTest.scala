package cuvee

import java.io.File
import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object VerifyTest extends TestSuite {
  test("cannot declare duplicate procedure parameters") {
    try {
      Check.checkProc("my-proc", Proc(List(Formal(Id("x"), Sort("Int")), Formal(Id("x"), Sort("Int"))), List(), True, True, Block(List())))
      fail("should have thrown an error")
    } catch {
      case e: Error => assertEquals(e.getMessage, "The method my-proc declares duplicate input parameters x")
    }
  }

  test("simple refinement verification condition") {
    val aState: List[Formal] = List(("counter", "Int"))
    var st = State.default.define(
      "abstract-counter",
      Obj(
        aState,
        Proc(List(), List(), True, True, "counter" := 0),
        List(
          (id("increment"), Proc(List(), List(), True, True, "counter" := (Id("counter") + 1))),
          (id("get-count"), Proc(List(), List[Formal](("count", "Int")), True, True, "count" := "counter")))))

    val cState: List[Formal] = List(("counter", "Int"))
    st = st.define("concrete-counter", Obj(
      cState,
      Proc(List(), List(), True, True, "counter" := 0),
      List(
        (id("increment"), Proc(List(), List(), True, True, "counter" := ("counter" - 1))),
        (id("get-count"), Proc(List(), List[Formal](("count", "Int")), True, True, "count" := (Num(0) - "counter"))))))

    val rel = DefineRefinement(("as", "abstract-counter"), ("cs", "concrete-counter"), "as_counter" === ((Num(0) - "cs_counter")))
    val vcs = Verify.verificationConditions(rel, st)

    // init
    assertEquals(vcs head, (And(List()) && And(List())) ==> (Num(0) === Num(0) - 0))

    // increment
    assertEquals(vcs(1), Forall(
      List(("as_counter", "Int"), ("cs_counter", "Int")),
      ("as_counter" === Num(0) - "cs_counter") // rel
        ==> (True // aPre
          ==> (True // cPre
            && (True // insEq
              ==> (True // outsEq
                && (Id("as_counter") + 1 === Num(0) - ("cs_counter" - 1)))))))) // rel

    // get-count
    assertEquals(vcs(2), Forall(
      List(("as_counter", "Int"), ("cs_counter", "Int")),
      ("as_counter" === Num(0) - "cs_counter") // rel
        ==> (True // aPre
          ==> (True // cPre
            && (True // insEq
              ==> (("as_counter" === Num(0) - "cs_counter") // outsEq
                && (Id("as_counter") === Num(0) - "cs_counter"))))))) // rel
  }

  test("refinement where inlining of post-relation class members does not work") {
    Expr._index = 0 // reset since we're testing against a specific index

    val aState: List[Formal] = List(("counter", "Int"))
    var st = State.default.define(
      "abstract-counter",
      Obj(
        aState,
        Proc(List(), List(), True, True, "counter" := 0),
        List((id("increment"), Proc(List(), List(), True, True, "counter" := (Id("counter") + 1))))))

    val cState: List[Formal] = List(("counter", "Int"))
    st = st.define("concrete-counter", Obj(
      cState,
      Proc(List(), List(), True, True, "counter" := 0),
      List((id("increment"), Proc(List(), List(), True, True, Spec(List("counter"), True, "counter" === (Old("counter") + 1)))))))

    val rel = DefineRefinement(("as", "abstract-counter"), ("cs", "concrete-counter"), "as_counter" === ((Num(0) - "cs_counter")))
    val vcs = Verify.verificationConditions(rel, st)

    assertEquals(vcs(1), Forall(
      List(("as_counter", "Int"), ("cs_counter", "Int")),
      ("as_counter" === Num(0) - "cs_counter") // rel
        ==> (True // aPre
          ==> (True // cPre
            && (True // insEq
              ==> (True // outsEq
                && Forall(List(("cs_counter1", "Int")), ("cs_counter1" === Id("cs_counter") + 1) ==> (True && Id("as_counter") + 1 === Num(0) - "cs_counter1")))))))) // rel
  }

  test("refinement for account") {
    val aState: List[Formal] = List(("balance", "Int"))
    var st = State.default.define("simple-account", Obj(
      aState,
      Proc(List(), List(), True, True, "balance" := 0),
      List(
        (id("deposit"), Proc(List[Formal](("amount", "Int")), List[Formal](("new-balance", "Int")), True, Id("amount") > 0, Block(List("balance" := (Id("balance") + "amount"), "new-balance" := "balance")))),
        (id("withdraw"), Proc(List[Formal](("amount", "Int")), List[Formal](("new-balance", "Int")), True, Id("amount") > 0 && Id("amount") <= "balance", Block(List("balance" := (Id("balance") - "amount"), "new-balance" := "balance")))))))

    val cState: List[Formal] = List(("debit", "Int"), ("credit", "Int"), ("max-overdraft", "Int"))
    st = st.define("double-account", Obj(
      cState,
      Proc(List[Formal](("maximum-overdraft", "Int")), List(), True, Id("maximum-overdraft") >= 0, Block(List("debit" := 0, "credit" := 0, "max-overdraft" := "maximum-overdraft"))),
      List(
        (id("deposit"), Proc(List[Formal](("amount", "Int")), List[Formal](("new-balance", "Int")), True, Id("amount") > 0, Block(List("credit" := (Id("credit") + "amount"), "new-balance" := Id("credit") - "debit")))),
        (id("withdraw"), Proc(List[Formal](("amount", "Int")), List[Formal](("new-balance", "Int")), True, Id("amount") > 0 && Id("amount") <= Id("credit") - "debit" + "max-overdraft", Block(List("debit" := (Id("debit") + "amount"), "new-balance" := Id("credit") - "debit")))))))

    val rel = DefineRefinement(("as", "simple-account"), ("cs", "double-account"), "as_balance" === Id("cs_credit") - "cs_debit")
    val vcs = Verify.verificationConditions(rel, st)

    vcs.foreach(vc => println(PrettyPrinter.printExpr(vc)))
  }

  test("absolute function verification condition") {
    val verificationCondition = Verify.verificationCondition(ParserTest.abs.proc, State.default, None)
    assertEquals(verificationCondition, Forall(List(Formal(Id("x"), Sort("Int"))), True ==> ((Id("x") < 0 ==> 0 - Id("x") >= 0) && !(Id("x") < 0) ==> Id("x") >= 0)))
  }

  def runUnwrappingErrors[A](fun: => A): A = {
    try {
      fun
    } catch {
      case any: Throwable =>
        printToStringStackTrace(any)
        throw any
    }
  }

  @scala.annotation.tailrec
  def printToStringStackTrace(t: Throwable): Unit = {
    println(t)
    if (t.getCause != null && t.getCause != t) {
      printToStringStackTrace(t.getCause)
    }
  }
}
