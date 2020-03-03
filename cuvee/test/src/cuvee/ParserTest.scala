package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object ParserTest extends TestSuite {
  val abs = DefineProc(Id("abs"), Proc(List(Formal(Id("x"), Sort("Int"))), List(Formal(Id("y"), Sort("Int"))),
    Id("true"),
    App(Id(">="), Id("y"), Num(0)),
    If(App(Id("<"), Id("x"), Num(0)), Assign(List(Pair(Id("y"), App(Id("-"), Num(0), Id("x"))))), Assign(List(Pair(Id("y"), Id("x")))))))

  test("parse number") {
    val number = Expr.from("2")
    assertEquals(number, Num(2))
  }

  test("parse assignment") {
    val assign = Prog.from("(assign (x 2))")
    assertEquals(assign, Assign(List(Pair(Id("x"), 2))))
  }

  test("parse procedure") {

    val proc = parseCmd("(define-proc abs ((x Int)) ((y Int))" +
      "(if (< x 0) (assign (y (- 0 x))) (assign (y x)))" +
      ":precondition true" +
      ":postcondition (>= y 0))")
    assertEquals(proc, abs)
  }

  test("parse procedure without pre- or postcondition") {
    val proc = parseCmd("(define-proc empty () () (block))")
    assertEquals(
      proc,
      DefineProc(
        Id("empty"),
        Proc(List(), List(), True, True)))
  }

  test("parse class") {
    val proc = parseCmd("(define-class counter-thing ((counter Int)) " +
      "(init () () (assign (counter 0)) :postcondition (= counter 0))" + ")")
    assertEquals(
      proc,
      DefineClass(
        "counter-thing",
        Obj(
          List(("counter", "Int")),
          Proc(List(), List(), True, "counter" === 0, "counter" := 0),
          Nil)))
  }

  test("parse comment") {
    assertEquals(parseCmds("; foo\n; bar"), List())
  }

  /* test("parse expectation of check-sat") {
    val parseable = new Parseable[CheckSat](Parser.check_sat_)
    val parse: String => CheckSat = str => VerifyTest.runUnwrappingErrors(parseable.from(str))

    assertEquals(parse("check-sat"), CheckSat())
    assertEquals(parse("check-sat :expect sat"), CheckSat(Some(Sat)))
    assertEquals(parse("check-sat :expect unsat"), CheckSat(Some(Unsat)))
    assertEquals(parse("check-sat :expect unknown"), CheckSat(Some(Unknown)))
  } */

  private def parseCmd(str: String): Cmd = {
    VerifyTest.runUnwrappingErrors(Cmd.from(str))
  }

  private def parseCmds(str: String): List[Cmd] = {
    VerifyTest.runUnwrappingErrors(Script.from(str))
  }
}
