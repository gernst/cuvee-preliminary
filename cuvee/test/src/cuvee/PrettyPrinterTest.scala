package cuvee

import cuvee.testutils.Implicits._
import cuvee.PrettyPrinter.{BinaryOperator, PrettyId, PrettyExpr}
import cuvee.test.TestSuite

object PrettyPrinterTest extends TestSuite {
  test("outer mult") {
    assertEquals(print(App("+", "a", "b") * App("+", "c", "d")), "(a + b) * (c + d)")
  }

  test("outer add") {
    assertEquals(print(App("*", "a", "b") + App("*", "c", "d")), "a * b + c * d")
  }

  test("forall") {
    assertEquals(print(Forall(List(("x", "Int")), App("f", "x") ==> (("x" || "a") && ("y" || "a")))), "∀ x: Int. f(x) ⟹ (x ∨ a) ∧ (y ∨ a)")
  }

  test("comma separator stays at the end of the line") {
    assertEquals(PrettyPrinter.breakIfNecessary(List(List("a"), List("a")), ", ", 3), List("a, ", "  a"))
  }

  private def print(expr: Expr): String = PrettyExpr(expr).print mkString "\n"
}
