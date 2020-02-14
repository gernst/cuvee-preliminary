package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._
import minitest.api.{AssertionException, SourceLocation}

object CheckTest extends TestSuite {
  val bool: Type = "Bool"
  val int: Type = "Int"

  test("Standard constants") {
    assertEquals(Check.infer("true", Map.empty, State.default), bool)
    assertEquals(Check.infer("false", Map.empty, State.default), bool)
    assertEquals(Check.infer(0, Map.empty, State.default), int)

    assertError(() => Check.infer("whatever", Map.empty, State.default), "unknown identifier")
  }

  test("Function applications") {
    assertEquals(Check.infer(Num(1) + 1, Map.empty, State.default), int)
    assertEquals(Check.infer(Id("true") && "false", Map.empty, State.default), bool)
    assertEquals(Check.infer((Id("true") && "false") || "true", Map.empty, State.default), bool)

    assertError(() => Check.infer(App("whatever", 1, "true"), Map.empty, State.default), "unknown function")
    assertError(() => Check.infer(Id("true") && 1, Map.empty, State.default), "signature")
    assertError(() => Check.infer((Id("true") && 1) || "true", Map.empty, State.default), "signature")
    assertError(() => Check.infer((Id("true") && "false") || 1, Map.empty, State.default), "signature")
  }

  test("Weakest precondition in bind with procedure call") {
    val st = State.default.define("int-bool-proc", Proc(List(Formal("in", int)), List(Formal("out", bool)), True, True, "out" := True))

    val expr = Forall(List(("x", "Int"), ("y", "Bool")), WP(Call("int-bool-proc", List("x"), List("y")), Id("y") === True))
    assertEquals(Check.infer(expr, Map.empty, st), Sort.bool)

    val unknownZ = Forall(List(("x", "Int"), ("y", "Bool")), WP(Call("int-bool-proc", List("x"), List("z")), Id("y") === True))
    assertError(() => Check.infer(unknownZ, Map.empty, st), "unknown identifier")

    val wrongOutputType = Forall(List(("x", "Int"), ("y", "Bool")), WP(Call("int-bool-proc", List("x"), List("x")), Id("y") === True))
    assertError(() => Check.infer(wrongOutputType, Map.empty, st), "signature")

    val wrongInputType = Forall(List(("x", "Int"), ("y", "Bool")), WP(Call("int-bool-proc", List("y"), List("y")), Id("y") === True))
    assertError(() => Check.infer(wrongInputType, Map.empty, st), "signature")

    val postConditionIsNotBoolean = Forall(List(("x", "Int"), ("y", "Bool")), WP(Call("int-bool-proc", List("x"), List("y")), Id("x")))
    assertError(() => Check.infer(postConditionIsNotBoolean, Map.empty, st), "post-condition")
  }

  private def assertError[T](fn: () => T, message: String)(implicit pos: SourceLocation): Unit = {
    try {
      fn()
      throw new AssertionException("Expected an error", pos)
    } catch {
      case e: AssertionException => throw e
      case e: Exception => if (e.getMessage == null || !e.getMessage.contains(message))
        throw new AssertionException(s"expected message containing '$message' but was '${e.getMessage}'", pos)
    }
  }
}
