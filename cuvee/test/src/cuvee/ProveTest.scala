package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object ProveTest extends TestSuite {
  test("boolean equals is expanded to two implications") {
    val st = State.default.declareConstants(List(("a", "Bool"), ("b", "Bool")))
    withSolver(Solver.default, solver => {
      solver.declare(("a", "Bool"))
      solver.declare(("b", "Bool"))
      val expr = Prove(solver, st).prove(Eq("a", And("b", True)))
      assertEquals(expr, Imp("a", "b") && Imp(And("b", True), "a"))
    })
  }

  test("function definitions are expanded when used as logical term") {
    val st = State.default.define("R", List(("x", "Int"), ("y", "Int")), "Bool",
      "x" === "y" && "x" === "x") // x = x will be discharged
    withSolver(Solver.default, solver => {
      val expr = Prove(solver, st).prove(Forall(List(("x", "Int"), ("y", "Int")), App("R", "x", "y")))
      assertEquals(expr, Forall(List(("x", "Int"), ("y", "Int")), "x" === "y"))
    })
  }

  test("recursive function definitions are not expanded") {
    val st = State.default.define("R", List(("x", "Int"), ("y", "Int")), "Bool",
      "x" === "y" && App("R", "x", "y"))
    withSolver(Solver.default, solver => {
      solver.declare("R", List("Int","Int"), "Bool")
      val initial = Forall(List(("x", "Int"), ("y", "Int")), App("R", "x", "y"))
      val expr = Prove(solver, st).prove(initial)
      assertEquals(expr, initial)
    })
  }
}
