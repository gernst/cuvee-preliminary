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
