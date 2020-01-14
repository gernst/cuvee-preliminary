package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object ParserTest extends TestSuite {
  val abs = DefineProc(Id("abs"), List(Formal(Id("x"), Sort("Int"))), List(Formal(Id("y"), Sort("Int"))),
    If(App(Id("<"), Id("x"), Num(0)), Assign(List(Pair(Id("y"), App(Id("-"), Num(0), Id("x"))))), Assign(List(Pair(Id("y"), Id("x"))))),
    Id("true"),
    App(Id(">="), Id("y"), Num(0))
  )

  test("parse number") {
    val number = new Parseable(Parser.expr).from("2")
    assertEquals(number, Num(2))
  }

  test("parse assignment") {
    val assign = new Parseable(Parser.prog).from("(assign (x 2))")
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
    assertEquals(proc, DefineProc(Id("empty"), List(), List(), Block(List()),
      True, True))
  }

  test("parse class") {
    val proc = parseCmd("(define-class counter-thing ((counter Int)) (" +
      "(define-proc init () () (assign (counter 0)) :postcondition (= counter 0))" +
      "))")
    assertEquals(proc, DefineClass("counter-thing", List(("counter", "Int")), List(DefineProc("init", List(), List(), "counter" := 0, True, "counter" === 0))))
  }

  private def parseCmd(str: String): Cmd = {
    VerifyTest.runUnwrappingErrors(new Parseable(Parser.cmd).from(str))
  }
}
