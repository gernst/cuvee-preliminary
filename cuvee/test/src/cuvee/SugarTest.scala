package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object SugarTest extends TestSuite {
  test("Minus remains fine when matched") {
    assertEquals(UMinus.unapply(App("-", "x")), Some(id("x")))
  }

  test("Plus goes ham") {
    val plus = Apps(List(id("+"), e"x", e"(+ x y (+ a  b))"))
    assertEquals(Plus.unapply(plus), Some(List(id("x"), id("x"), id("y"), id("a"), id("b"))));
  }

  test("Implies is right-associative") {
    val args = List(e"(=> a b)", e"c", e"d")
    val imp = Apps(Id.imp :: args)
    assertEquals(imp, App("=>", "a", "b") ==> App("=>", "c", "d"))
  }
}
