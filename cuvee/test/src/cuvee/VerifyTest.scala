package cuvee

import java.io.File

import cuvee.test.TestSuite

object VerifyTest extends TestSuite {
  for (file <- List(
    "examples/verifications/abs-proc.smt2",
    "examples/verifications/gcd-proc.smt2",
    "examples/verifications/zero-proc.smt2",
    "examples/refinements/accounts.smt2")) {
    test("verify " + file) {
      Expr._index = 0
      val in = new File(file)
      val source: Source = runUnwrappingErrors(Verify(Source.file(in).cmds))

      println(s"Verification condition for $file")
      println()
      Cuvee.run(source, Solver.stdout, (_: Res) => { })
      println()

      val report = prove_!
      val solver = Solver.z3()
      Cuvee.run(source, solver, report)
    }
  }

  test("absolute function verification condition") {
    val verificationCondition = Verify.verificationCondition(ParserTest.abs, State.default, None)
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
