package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object SugarTest extends TestSuite {
  test("Minus remains fine when matched") {
    val right: List[Expr] = List("a", "b", e"(- c d)")
    val wrong: List[Expr] = List("a", "b", "c", "d")
    Minus.nary(List(e"(- a b)", e"(- c d)")) match {
      case Minus.nary(`wrong`) =>
        fail()
      case Minus.nary(`right`) =>
      // good
      case _ =>
        fail()
    }
  }

  test("Implies is right-associative") {
    val args = List(e"(=> a b)", e"c", e"d")
    val imp = Imp.nary(args)
    assertEquals(imp, App("=>", "a", "b") ==> App("=>", "c", "d"))
    assertEquals(Imp.flatten(imp.args), args)
  }
}
