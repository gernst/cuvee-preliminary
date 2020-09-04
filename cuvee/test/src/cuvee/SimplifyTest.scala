package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object SimplifyTest extends TestSuite {
  test("remove from plus") {
    assertEquals(simplifyInt(e"(= (+ a b) (+ a c))"), e"(= b c)")
  }

  test("remove from minus") {
    assertEquals(simplifyInt(e"(= (- a b) (- a c))"), e"(= c b)") // Note: swaps sides
  }

  test("remove from gt") {
    assertEquals(simplifyInt(e"(> (- a b) (- a c))"), e"(< b c)")
  }

  test("double minus") {
    assertEquals(simplifyInt(e"(= 0 (- (- x)))"), e"(= 0 x)")
  }

  test("flatten addition") {
    assertEquals(Simplify.norm(e"(= (+ balance (- 0 amount)) (+ credit (- 0 (+ debit amount))))"), e"(= (+ balance debit) credit)")
  }

  test("simplify linear") {
    val simplified = Simplify.norm(e"(= (+ x x) x)")
    assertEquals(simplified, e"(= x 0)")
  }

  test("don't replace minus without equation") {
    assertEquals(Simplify.norm(e"(- a b)"), e"(- a b)")
  }

  test("don't replace minus in equation when it is an argument to an unknown function") {
    assertEquals(Simplify.norm(e"(= 0 (f (- a b)))"), e"(= 0 (f (- a b)))")
  }

  test("extract from bind") {
    val nested =
      e"""(forall ((name Name) (|name'| Name)) (=>
        (and (= |name'| name) (= (select fs name) |undef_f|) true)
        (forall ((name3 Name)) (=> (= name name3)
          (=> (= |name'| name3)
            (or
              (= (select (store fs name empty) name3) |undef_f|)
              (= (select (store fs name empty) name3) (select disk (select (store index |name'| null) name3)))))))))"""
    val normed = Simplify.norm(nested)
    assertEquals(normed, e"(or (or (forall ((name Name)) (not (= (select fs name) |undef_f|))) (= empty |undef_f|)) (= empty (select disk null)))")
  }

  private def simplifyInt(phi: Expr): Expr = {
    val formals = phi.free.map(Formal(_, Sort.int)).toList
    simplify(phi, formals)
  }

  private def simplify(phi: Expr, formals: List[Formal]) = {
    withSolver(Solver.z3(), solver => {
      solver.bind(formals)
      val simplifier = Simplify(solver)
      val simplified = simplifier(List(phi))
      assertEquals(simplified.size, 1)
      simplified.head
    })
  }
}