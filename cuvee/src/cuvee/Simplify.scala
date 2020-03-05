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

  def assuming(pre: Expr, post: Expr): Expr = backend.scoped {
    backend assert pre
    prove(post)
  }

  def scoped(bound: List[Formal], post: Expr): Expr = backend.scoped {
    backend bind bound
    prove(post)
  }

  def prove(phi: Expr): Expr = phi match {
    case _ if backend isFalse phi =>
      False
    case _ if backend isTrue phi =>
      True
    case Imp(phi, psi) =>
      assuming(phi, psi)
    case And.nary(args) =>
      and(args map prove)
    case Or.nary(args) =>
      or(args map prove)
    case Forall(bound, body) =>
     scoped(bound, body)
    case _ =>
      phi
  }
}

object Simplify {
  var debug = false
  var qe = true

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
    // push in negation
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
    case Not(Distinct(List(left, right))) =>
      eq(norm(left), norm(right))

    case Imp(phi, psi) =>
      norm(!phi || psi)
    case And.nary(args) =>
      and(norm(args))
    case Or.nary(args) =>
      or(norm(args))
    case bind: Bind =>
      normQuant(bind)

    // eliminate all but unary minuses
    case Minus.nary(Nil) => 0
    case Minus.nary(List(Minus.nary(List(single)))) => norm(single)
    case Minus.nary(List(Plus.nary(args))) => norm(Plus.nary(args.map(arg => Minus.nary(List(arg)))))
    case Minus.nary(List(single)) => Minus.nary(List(norm(single)))
    case Minus.nary(head :: tail) => norm(Plus.nary(head :: tail.map(t => Minus.nary(List(t)))))

    case Plus.nary(Nil) => 0
    case Plus.nary(List(single)) => norm(single)
    case Plus.nary(args) => Plus.nary(Plus.flatten(args map norm) map norm)


    case Not(Lt(a, b)) => linear(Le, b, a)
    case Not(Le(a, b)) => linear(Lt, b, a)
    case Not(Gt(a, b)) => linear(Le, a, b)
    case Not(Ge(a, b)) => linear(Lt, a, b)

    case Gt(a, b) => linear(Lt, b, a)
    case Ge(a, b) => linear(Le, b, a)

    case Lt(a, b) => linear(Lt, a, b)
    case Le(a, b) => linear(Le, a, b)
    case Eq(a, b) => linear(Eq, a, b)

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

    case _ =>
      expr
  }

  private def normQuant(bind: Bind): Expr = {
    val Bind(quant, _, _) = bind
    (if (Simplify.qe) eliminateBindings(bind) else bind) match {
      case Bind(_, formals, body) =>
        val body_ = norm(body)
        if (Simplify.qe && body_ != body) {
          // might have to eliminate bindings again
          return norm(quant(formals, body_))
        }
        body_ match {
          case boolean @ App(id @ (Id.and | Id.or), args) => {
            val unbound = args.filter(_.free.disjoint(bind.bound))
            if (unbound.isEmpty) {
              Bind(quant, formals, boolean)
            } else {
              val bound = args.filterNot(_.free.disjoint(bind.bound))
              App(id, quant(formals, App(id, bound)) :: unbound)
            }
          }
          case other => Bind(quant, formals, other)
        }
      case other => norm(other)
    }
  }

  def linear[T <: Expr](eq: (Expr, Expr) => T, left: Expr, right: Expr): Expr = {
    (norm(left), norm(right)) match {
      case (Plus.nary(lArgs), Plus.nary(rArgs)) =>
        val common = lArgs.intersect(rArgs)
        (lArgs.filterNot(common.contains), rArgs.filterNot(common.contains)) match {
          case (List(Minus.nary(List(lArg: Expr))), List(Minus.nary(List(rArg: Expr)))) =>
            reverseEq(eq, lArg, rArg)
          case (left_, right_) => eq(plus(left_), plus(right_))
        }
      case (Minus.nary(List(lArg: Expr)), Minus.nary(List(rArg: Expr))) =>
        reverseEq(eq, lArg, rArg)
      case (left_, right_) if left_ == right_ => True
      case (left_, right_) => eq(left_, right_)
    }
  }

  def reverseEq[T](eq: (Expr, Expr) => T, left: Expr, right: Expr): T = eq match {
    case Eq => eq(left, right)
    case _ => eq(right, left)
  }

  def plus(args: List[Expr]): Expr = args match {
    case Nil => 0
    case single :: Nil => single
    case more => Plus.nary(more)
  }

  def norm(exprs: List[Expr]): List[Expr] = {
    exprs map norm
  }

  def eliminateBindings(phi: Bind): Expr = phi match {
    case Bind(quant, formals, body) => body match {
      case And.nary(args) => quant(formals, Or.nary(eliminateBindingsFromNary(formals, args, false)))
      case Or.nary(args) => quant(formals, Or.nary(eliminateBindingsFromNary(formals, args, true)))
      case _ => phi
    }
  }

  def eliminateBindingsFromNary(formals: List[Formal], args: List[Expr], neg: Boolean): List[Expr] = {
    for (arg <- args) {
      arg match {
        case Not(Eq(id: Id, expr)) if neg && formals.ids.contains(id) =>
          return eliminateBindingsFromNary(formals, args filter (_ != arg) map (_.subst(Map(id -> expr))), neg)
        case Not(Eq(expr, id: Id)) if neg && formals.ids.contains(id) =>
          return eliminateBindingsFromNary(formals, args filter (_ != arg) map (_.subst(Map(id -> expr))), neg)
        case Eq(id: Id, right) if !neg && formals.ids.contains(id) =>
          return eliminateBindingsFromNary(formals, args filter (_ != arg) map (_.subst(Map(id -> right))), neg)
        case Eq(left, id: Id) if !neg && formals.ids.contains(id) =>
          return eliminateBindingsFromNary(formals, args filter (_ != arg) map (_.subst(Map(id -> left))), neg)
        case _ =>
      }
    }
    args
  }
}
