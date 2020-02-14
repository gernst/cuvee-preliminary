package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._
import minitest.api.{AssertionException, SourceLocation}

object CheckTest extends TestSuite {
  val bool: Type = "Bool"
  val int: Type = "Int"

  test("Standard constants") {
    assertEquals(Check.infer(e"true", Map.empty, State.default), bool)
    assertEquals(Check.infer(e"false", Map.empty, State.default), bool)
    assertEquals(Check.infer(e"0", Map.empty, State.default), int)

    assertError(() => Check.infer(e"whatever", Map.empty, State.default), "unknown identifier")
  }

  test("Function applications") {
    assertEquals(Check.infer(e"(+ 1 1)", Map.empty, State.default), int)
    assertEquals(Check.infer(e"(and true false)", Map.empty, State.default), bool)
    assertEquals(Check.infer(e"(or (and true false) true)", Map.empty, State.default), bool)

    assertError(() => Check.infer(e"(whatever 1 true)", Map.empty, State.default), "unknown function")
    assertError(() => Check.infer(e"(and true 1)", Map.empty, State.default), "signature")
    assertError(() => Check.infer(e"(or (and true 1) true)", Map.empty, State.default), "signature")
    assertError(() => Check.infer(e"(or (and true false) 1)", Map.empty, State.default), "signature")
  }

  test("Weakest precondition in bind with procedure call") {
    val st = State.default.define("int-bool-proc", Proc(List(Formal("in", int)), List(Formal("out", bool)), True, True, "out" := True))

    assertEquals(Check.infer(e"(forall ((x Int) (y Bool)) (wp (call int-bool-proc (x) (y)) (= y true)))", Map.empty, st), Sort.bool)

    assertError(() => Check.infer(e"(forall ((x Int) (y Bool)) (wp (call int-bool-proc (x) (z)) (= y true)))", Map.empty, st), "unknown identifier")

    assertError(() => Check.infer(e"(forall ((x Int) (y Bool)) (wp (call int-bool-proc (x) (x)) (= y true)))", Map.empty, st), "signature")

    assertError(() => Check.infer(e"(forall ((x Int) (y Bool)) (wp (call int-bool-proc (y) (y)) (= y true)))", Map.empty, st), "signature")

    assertError(() => Check.infer(e"(forall ((x Int) (y Bool)) (wp (call int-bool-proc (x) (y)) x))", Map.empty, st), "post-condition")
  }

  test("Break") {
    assertError(() => Check.checkProg(p"(block (break))", Map.empty, State.default, false), "break");

    Check.checkProg(p"(while true (break))", Map.empty, State.default, false);
  }

  test("Let") {
    assertEquals(Check.infer(e"(let ((x true)) (x))", Map.empty, State.default), bool)

    assertEquals(Check.infer(e"(let ((x y)) (x))", Map(Id("y") -> bool), State.default), bool)

    assertError(() => Check.infer(e"(let ((x y)) (x))", Map.empty, State.default), "unknown identifier")
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
