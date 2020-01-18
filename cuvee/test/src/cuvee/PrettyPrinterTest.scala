package cuvee

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

  private implicit def id(s: String): Id = Id(s)

  private implicit def formal(f: (String, String)): Formal = Formal(Id(f._1), Sort(f._2))

  private def print(expr: Expr): String = PrettyExpr(expr).print mkString "\n"
}
