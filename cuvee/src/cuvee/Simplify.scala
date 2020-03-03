package cuvee

case class Simplify(backend: Solver) {
  import Simplify._

  def apply(phi: Expr): Expr = {
    and(apply(List(phi)))
  }

  def apply(phis: List[Expr]): List[Expr] = {
    val _phis = norm(phis)

    if (Simplify.debug) {
      println("input formulas:")
      for (phi <- phis)
        println(Printer.format(phi, "  "))
      println("negation normal form:")
      for (phi <- _phis)
        println(Printer.format(phi, "  "))
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

  def eliminateBindings(phi: Expr): Expr = {
    phi match {
      case Bind(quant, formals, body) => {
        body match {
          case Or.nary(args) => quant(formals, Or.nary(eliminateBindingsFromDisjunction(formals, args)))
          case other => other
        }
      }
      case other => other
    }
  }

  def eliminateBindingsFromDisjunction(formals: List[Formal], args: List[Expr]): List[Expr] = {
    for (arg <- args) {
      arg match {
        case Not(Eq(id: Id, right)) if formals.ids contains id =>
          return eliminateBindingsFromDisjunction(formals, args filter (_ != arg) map (_.subst(Map(id -> right))))
        case Not(Eq(left, id: Id)) if formals.ids contains id =>
          return eliminateBindingsFromDisjunction(formals, args filter (_ != arg) map (_.subst(Map(id -> left))))
        case any =>
      }
    }
    args
  }

  def nary(todo: List[Expr], rdone: List[Expr], neg: Boolean, top: Boolean, changed: Boolean = false): List[Expr] = todo match {
    case Nil =>
      val done = rdone.reverse
      if (changed) nary(done, Nil, neg, top)
      else done

    // don't simplify top-level axioms (rarely useful)
    case (phi @ Forall(_, _)) :: rest if top =>
      assert(!neg)
      // val phi_ = eliminateBindings(phi)
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
    case _ if backend isFalse phi =>
      False
    case _ if backend isTrue phi =>
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
}

object Simplify {
  var debug = false

  def eq(left: Expr, right: Expr) = {
    if (left == right) True
    else Eq(left, right)
  }

  def distinct(args: List[Expr]) = args match {
    case _ if args.hasDuplicates =>
      False
    case List(Id.nil, Cons(_, _)) =>
      True
    case List(Cons(_, _), Id.nil) =>
      True
    case _ =>
      Distinct(args)
  }

  def not(phi: Expr): Expr = phi match {
    case True => False
    case False => True
    case Not(psi) => psi
    case _ => Not(phi)
  }

  def and(args: List[Expr]): Expr = {
    val _args = And.flatten(args)
    if (_args contains False) False
    else And(_args.distinct filter (_ != True))
  }

  def or(args: List[Expr]): Expr = {
    val _args = Or.flatten(args)
    if (_args contains True) True
    else Or(_args.distinct filter (_ != False))
  }

  def norm(expr: Expr): Expr = expr match {
    case Not(True) =>
      False
    case Not(False) =>
      True
    case Not(Not(phi)) =>
      norm(phi)
    case Not(Imp(phi, psi)) =>
      norm(phi && !psi)
    case Not(And.nary(args)) =>
      or(norm(Not(args)))
    case Not(Or.nary(args)) =>
      and(norm(Not(args)))
    case Not(Bind(quant, formals, body)) =>
      Bind(!quant, formals, norm(!body))

    case Imp(phi, psi) =>
      norm(!phi || psi)
    case And.nary(args) =>
      and(norm(args))
    case Or.nary(args) =>
      or(norm(args))
    case Bind(quant, formals, body) =>
      Bind(quant, formals, norm(body))

    case Not(Lt(a, b)) => Le(b, a)
    case Not(Le(a, b)) => Lt(b, a)
    case Not(Gt(a, b)) => Le(a, b)
    case Not(Ge(a, b)) => Lt(a, b)

    case Gt(a, b) => Lt(b, a)
    case Ge(a, b) => Le(b, a)

    case Head(Cons(x, xs)) => x
    case Tail(Cons(x, xs)) => xs

    case Distinct(List(Cons(_, _), Id.nil)) =>
      True
    case Distinct(List(Id.nil, Cons(_, _))) =>
      True

    case Distinct(args) =>
      distinct(norm(args))

    case App(fun, args) =>
      App(fun, norm(args))

    case Eq(left, right) =>
      eq(norm(left), norm(right))

    case _ =>
      expr
  }

  def norm(exprs: List[Expr]): List[Expr] = {
    exprs map norm
  }
}
