package cuvee

case class Simplify(state: State) {
  val backend = Solver.default
  state replay backend

  import Simplify._

  def apply(goal: Goal): Goal = {
    val Goal(bound, asserts, concls, goals) = goal
    backend.scoped {
      backend.bind(bound)
      for (ctx <- asserts)
        backend.assert(ctx)

      val _concls = concls flatMap {
        case concl if isTrue(concl) => None
        case concl if isFalse(concl) => Some(False)
        case concl => Some(concl)
      }

      val _goals = goals flatMap {
        goal =>
          val _goal = apply(goal)
          if(_goal.isClosed) None
          else Some(_goal)
      }

      Goal(bound, asserts, _concls, _goals)
    }
  }

  def apply(phis: List[Expr]): List[Expr] = {
    val _phis = Expr.nnf(phis)

    if (Simplify.debug) {
      println("simplify at top-level:")
      for (phi <- _phis)
        println(s"  $phi")
    }
    val _res = top(_phis)
    And.flatten(_res)
  }

  def top(args: List[Expr]): List[Expr] = {
    nary(todo = args, rdone = Nil, neg = false, top = true)
  }

  def con(args: List[Expr]): List[Expr] = {
    nary(todo = args, rdone = Nil, neg = false, top = false)
  }

  def dis(args: List[Expr]): List[Expr] = {
    nary(todo = args, rdone = Nil, neg = true, top = false)
  }

  def _assert(phi: Expr, neg: Boolean) = {
    if (neg)
      backend.assert(!phi)
    else
      backend.assert(phi)
  }

  def nary(todo: List[Expr], rdone: List[Expr], neg: Boolean, top: Boolean, changed: Boolean = false): List[Expr] = todo match {
    case Nil =>
      val done = rdone.reverse
      if (changed) nary(done, Nil, neg, top)
      else done

    // don't simplify top-level axioms (rarely useful)
    case (phi @ Forall(_, _)) :: rest if top =>
      assert(!neg)
      nary(rest, phi :: rdone, neg, top, changed)

    case phi :: rest =>
      val _phi = backend.scoped {
        for (ctx <- rest) _assert(ctx, neg)
        for (ctx <- rdone) _assert(ctx, neg)
        simplify(phi)
      }

      nary(rest, _phi :: rdone, neg, top, _phi != phi || changed)
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
      and(_args)
    case Or.nary(args) =>
      val _args = dis(args)
      or(_args)
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
    val _args = And.flatten(args)
    if (_args contains False) False
    else And(_args filter (_ != True))
  }

  def or(args: List[Expr]) = {
    val _args = Or.flatten(args)
    if (_args contains True) True
    else Or(_args filter (_ != False))
  }
}
