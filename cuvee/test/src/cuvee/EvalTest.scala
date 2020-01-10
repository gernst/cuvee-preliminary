package cuvee

import cuvee.test.TestSuite

object EvalTest extends TestSuite {
  private val a: Id = Id("a")
  private val b: Id = Id("b")
  private val x: Id = Id("x")
  private val y: Id = Id("y")
  private val int: Sort = Sort("Int")

  test("wp old value with double reassign") {
    // x = x * 2; x = x * 2; y = x;
    val code = Block(List(assign(x, x * 2), assign(x, x * 2), assign(y, x)))

    val wp = WP(code, y === Old(x) * 2 * 2)

    val env = Env.empty.bind(List(Formal(x, int), Formal(y, int)))
    val evald = Eval.eval(wp, env, List(env), State.default).asInstanceOf[Eq]

    assertEquals(evald.left, x * 2 * 2)
    assertEquals(evald.left, evald.right)
  }

  test("function call") {
    // define function forward(x) = x
    val state = State.default.define(DefineProc(Id("forward"), formal(a, int), formal(b, int), assign(b, a), True, Eq(b, a)))
    val env = Env.empty.bind(formal(x, int) ++ formal(y, int))
    val code = CallProc(Id("forward"), List(x * 2), List(y))
    val wp = WP(code, y === Old(x) * 2)
    val evald = Eval.eval(wp, env, List(env), state)
    println(evald)
  }

  def formal(id: Id, sort: Sort) = List(Formal(id, sort))

  def assign(id: Id, expr: Expr) = Assign(List(Pair(id, expr)))
}
