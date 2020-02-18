package cuvee

case class Simplify(state: State) {
  val backend = Solver.default
  state replay backend

  import Simplify._

  def apply(phis: List[Expr]): List[Expr] = {
    val _phis = Expr.nnf(phis)
    con(_phis)
  }

  def con(args: List[Expr]): List[Expr] = {
    nary(todo = args, rdone = Nil, neg = false)
  }

  def dis(args: List[Expr]): List[Expr] = {
    nary(todo = args, rdone = Nil, neg = true)
  }

  def _assert(phi: Expr, neg: Boolean) = {
    if (neg)
      backend.assert(!phi)
    else
      backend.assert(phi)
  }

  def nary(todo: List[Expr], rdone: List[Expr], neg: Boolean, changed: Boolean = false): List[Expr] = todo match {
    case Nil =>
      val done = rdone.reverse
      if (changed) nary(done, Nil, neg)
      else done

    case phi :: rest =>
      val _phi = backend.scoped {
        for (ctx <- rest) _assert(ctx, neg)
        for (ctx <- rdone) _assert(ctx, neg)
        simplify(phi)
      }

      nary(rest, _phi :: rdone, neg, _phi != phi || changed)
  }

  def simplify(phi: Expr): Expr = {
    val (ms, _phi) = time(_simplify(phi))
    if (Simplify.debug) {
      println(s"simplify (${ms}ms)")
      println("     " + phi)
      println("  ~> " + _phi)
      println()
    }
    _phi
  }

  def _simplify(phi: Expr): Expr = phi match {
    case _ if isFalse(phi) =>
      False
    case _ if isTrue(phi) =>
      True
    case And.nary(args) =>
      val _args = con(args)
      And(_args)
    case Or.nary(args) =>
      val _args = dis(args)
      Or(_args)
    case _ =>
      phi
  }

  def isTrue(phi: Expr) = {
    val res = (phi == True) || (backend isUnsat !phi)
    res
  }

  def isFalse(phi: Expr) = {
    val res = (phi == False) || (backend isUnsat phi)
    res
  }
}

object Simplify {
  var debug = false

  def eq(left: Expr, right: Expr): Expr = {
    if (left == right) True
    else Eq(left, right)
  }

  def not(phi: Expr) = phi match {
    case False => True
    case True => False
    case App(Id.not, List(phi)) => phi
    case _ => !phi
  }

  def imp(phi: Expr, psi: Expr) = (phi, psi) match {
    case (False, _) => True
    case (True, _) => psi
    case (_, False) => not(phi)
    case (_, True) => True
    case _ => phi ==> psi
  }

  def and(phi: Expr, psi: Expr) = (phi, psi) match {
    case (False, _) => False
    case (True, _) => psi
    case (_, False) => False
    case (_, True) => phi
    case _ => phi && psi
  }

  def or(phi: Expr, psi: Expr) = (phi, psi) match {
    case (False, _) => psi
    case (True, _) => True
    case (_, False) => phi
    case (_, True) => True
    case _ => phi || psi
  }

  def and(args: List[Expr]) = {
    if (args contains False) False
    else And(args filter (_ != True))
  }

  def or(args: List[Expr]) = {
    if (args contains True) True
    else Or(args filter (_ != False))
  }
}
