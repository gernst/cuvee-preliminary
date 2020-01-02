package cuvee

import cuvee.test.TestSuite

object CmdTest extends TestSuite {
  test("cannot declare duplicate procedure parameters") {
    try {
      DefineProc(Id("my-proc"), List(Formal(Id("x"), Sort("Int")), Formal(Id("x"), Sort("Int"))), List(), Block(List()), True, True)
      fail("should have thrown an error")
    } catch {
      case e: Error => assertEquals(e.info.toString, "The method my-proc declares duplicate input parameters x")
    }
  }

  test("absolute function verification condition") {
    val verificationCondition = ParserTest.abs.verificationCondition(State.default)
    println(verificationCondition)
  }
}
