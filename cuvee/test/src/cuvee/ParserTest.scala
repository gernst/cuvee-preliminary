package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object ParserTest extends TestSuite {
  val abs = DefineProc(Id("abs"), Proc(List(Formal(Id("x"), Sort("Int"))), List(Formal(Id("y"), Sort("Int"))),
    Id("true"),
    App(Id(">="), Id("y"), Num(0)),
    If(App(Id("<"), Id("x"), Num(0)), Assign(List(Pair(Id("y"), App(Id("-"), Num(0), Id("x"))))), Assign(List(Pair(Id("y"), Id("x")))))))

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
    assertEquals(
      proc,
      DefineProc(
        Id("empty"),
        Proc(List(), List(), True, True, Block(List()))))
  }

  test("parse class") {
    val proc = parseCmd("(define-class counter-thing ((counter Int)) " +
      "(define-proc init () () (assign (counter 0)) :postcondition (= counter 0))" + ")")
    assertEquals(
      proc,
      DefineClass(
        "counter-thing",
        Obj(
          List(("counter", "Int")),
          Proc(List(), List(), True, "counter" === 0, "counter" := 0),
          Nil)))
  }

  test("parse refinement") {
    val proc = parseCmd("(refinement (as abstract-class) (cs concrete-class) (= cs_member (- 0 as_member)))")
    assertEquals(proc, DefineRefinement(("as", "abstract-class"), ("cs", "concrete-class"), "cs_member" === (Num(0) - "as_member")))
  }

  test("parse comment") {
    assertEquals(parseCmds("; foo\n; bar"), List())
  }

  private def parseCmd(str: String): Cmd = {
    VerifyTest.runUnwrappingErrors(new Parseable(Parser.cmd).from(str))
  }

  private def parseCmds(str: String): List[Cmd] = {
    VerifyTest.runUnwrappingErrors(new Parseable(Parser.cmd*).from(str))
  }
}
