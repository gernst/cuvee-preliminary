package cuvee

sealed trait Tactic

object Tactic {
  case object split extends Tactic
}

case class Prove(backend: Solver) {
  import Simplify._

  def apply(phi: Expr): Expr = {
    prove(phi)
  }

  def prove(todo: List[Expr], neg: Boolean): List[Expr] = todo match {
    case Nil =>
      Nil
    case phi :: rest =>
      val _phi = prove(phi)
      val __phi = if (neg) !_phi else _phi
      val _rest = backend.asserting(__phi) { prove(rest, neg) }
      _phi :: _rest
  }

  def prove(phi: Expr): Expr = phi match {
    /* case _ if backend isFalse phi =>
      False */
    case Imp(phi, psi) =>
      val _psi = backend.asserting(phi) { prove(psi) }
      imp(phi, _psi)
    case And(args) =>
      val _args = prove(args, neg = false)
      and(_args)
    case Forall(bound, body) =>
      val _body = backend.binding(bound) { prove(body) }
      forall(bound, _body)
    case _ if backend isTrue phi =>
      True
    case _ =>
      phi
  }
}