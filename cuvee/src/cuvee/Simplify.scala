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
    case (phi@Forall(_, _)) :: rest if top =>
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
    case And(args) =>
      val _args = con(args)
      and(_args)
    case Or(args) =>
      val _args = dis(args)
      or(_args)
    case _ =>
      phi
  }
}

object Simplify {
  var debug = false
  var qe = true

  def eq(left: Expr, right: Expr) = (left, right) match {
    case _ if left == right => True
    case _ => Eq(left, right)
  }

  def lt(left: Expr, right: Expr) = {
    if (left == right) False
    else Lt(left, right)
  }

  def le(left: Expr, right: Expr) = {
    if (left == right) True
    else Le(left, right)
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
    case Eq(left, right) => Distinct(List(left, right))
    case _ => Not(phi)
  }

  def plus(arg1: Expr, arg2: Expr): Expr = {
    plus(List(arg1, arg2))
  }

  def plus(args: List[Expr]): Expr = {
    val _args = Plus.flatten(args)
    Plus(_args filter (_ != Num.zero))
  }


  def ite(test: Expr, left: Expr, right: Expr): Expr = (test, left, right) match {
    case (True, left, right) => left
    case (False, left, right) => right
    case _ => Ite(test, left, right)
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

  def forall(bound: List[Formal], body: Expr) = {
    bind(Forall, bound, body)
  }

  def exists(bound: List[Formal], body: Expr) = {
    bind(Exists, bound, body)
  }

  def bind(quant: Quant, bound: List[Formal], body: Expr): Expr = body match {
    case Bind(`quant`, bound_, body_) =>
      bind(quant, bound ++ bound_, body_)
    case _ =>
      quant(bound, body)
  }

  def imp(phi: Expr, psi: Expr): Expr = (phi, psi) match {
    case (False, _) => True
    case (_, True) => True
    case (_, False) => !phi
    case (True, _) => psi
    case _ => Imp(phi, psi)
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
    case Not(And(args)) =>
      or(norm(args map (!_)))
    case Not(Or(args)) =>
      and(norm(args map (!_)))
    case Not(Bind(quant, formals, body)) =>
      normQuant(Bind(!quant, formals, !body))
    case Not(Distinct(List(left, right))) =>
      eq(norm(left), norm(right))

    case Imp(phi, psi) =>
      norm(!phi || psi)
    // imp(norm(phi), norm(psi))
    case And(args) =>
      and(norm(args))
    case Or(args) =>
      or(norm(args))
    case bind: Bind =>
      normQuant(bind)

    case UMinus(UMinus(arg)) => norm(arg)
    case UMinus(Plus(args)) => norm(Plus(args map (-_)))
    case UMinus(Minus(a, b)) => norm(-a + b)

    case Plus(args) => plus(norm(args))

    case Not(Lt(a, b)) => linear(le, poly(b), poly(a))
    case Not(Le(a, b)) => linear(lt, poly(b), poly(a))
    case Not(Gt(a, b)) => linear(le, poly(a), poly(b))
    case Not(Ge(a, b)) => linear(lt, poly(a), poly(b))

    case Gt(a, b) => linear(lt, poly(b), poly(a))
    case Ge(a, b) => linear(le, poly(b), poly(a))

    case Lt(a, b) => linear(lt, poly(a), poly(b))
    case Le(a, b) => linear(le, poly(a), poly(b))
    case Eq(a, b) => maybeLinear(eq, poly(a), poly(b))

    case Head(Cons(x, _)) => norm(x)
    case Tail(Cons(_, xs)) => norm(xs)

    case Distinct(List(Cons(_, _), Id.nil)) =>
      True
    case Distinct(List(Id.nil, Cons(_, _))) =>
      True

    case Distinct(args) =>
      distinct(norm(args))

    case App(fun, args) =>
      App(fun, norm(args))

    case Select(Store(_, index, value), index2) if index == index2 =>
      norm(value)

    case s@Select(array, index) =>
      // norm arguments to attempt the shortcut above but prevent infinite recursion
      val s_ = Select(norm(array), norm(index))
      if (s != s_) norm(s_) else s

    case _ =>
      expr
  }

  /** Normalize into polynomial form (in the context of an (in-)equality */
  def poly(expr: Expr): Expr = expr match {
    // New case here
    case Minus(a, b) => plus(poly(a), poly(-b))

    // Same as in norm but recurse with poly
    case UMinus(UMinus(arg)) => poly(arg)
    case UMinus(Plus(args)) => poly(Plus(args map (-_)))
    case UMinus(Minus(a, b)) => poly(-a + b)

    case Plus(args) => plus(args map poly)

    case _ => norm(expr)
  }

  def norm(exprs: List[Expr]): List[Expr] = {
    exprs map norm
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
          case And(args) => extractUnbound(bind, args, Simplify.and)
          case Or(args) => extractUnbound(bind, args, Simplify.or)
          case other => Simplify.bind(quant, formals, other)
        }
      case other => norm(other)
    }
  }

  private def extractUnbound(bind: Bind, args: List[Expr], app: List[Expr] => Expr) = {
    val Bind(quant, formals, _) = bind
    val unbound = args.filter(_.free.disjoint(bind.bound))
    if (unbound.isEmpty) {
      quant(formals, app(args))
    } else {
      val bound = args.filterNot(_.free.disjoint(bind.bound))
      app(quant(formals, app(bound)) :: unbound)
    }
  }

  def partitionSum(args: List[Expr]) = partition(args) {
    case UMinus(arg) => Right(arg)
    case arg => Left(arg)
  }

  def maybeLinear(op: (Expr, Expr) => Expr, left: Expr, right: Expr): Expr = (left, right) match {
    case (App(Id.plus, _), _) => linear(eq, left, right)
    case (_, App(Id.plus, _)) => linear(eq, left, right)
    case _ => op(left, right)
  }

  def linear(op: (Expr, Expr) => Expr, left: Expr, right: Expr): Expr = {
    // Note: don't use val Plus(lefts) = left
    //       because that requires at least one +
    val l1 = Plus.flatten(left)
    val r1 = Plus.flatten(right)

    // Partition into positive and negative terms
    val (pl, ml) = partitionSum(l1)
    val (pr, mr) = partitionSum(r1)

    // Swap negative terms to other side
    val l2 = pl ++ mr
    val r2 = pr ++ ml

    // count occurrences
    val l3 = l2 groupBy identity mapValues (_.size)
    val r3 = r2 groupBy identity mapValues (_.size)

    // find common occurrences
    val common: Map[Expr, Int] = ((l3.keys ++ r3.keys) toList).distinct map
      ((k: Expr) => k -> Math.min(l3 getOrElse(k, 0), r3 getOrElse(k, 0))) toMap

    // subtract common occurences on both sides
    val l4 = l3 map (e => e._1 -> (e._2 - common(e._1)))
    val r4 = r3 map (e => e._1 -> (e._2 - common(e._1)))

    // repeat terms according to occurrences
    val l5 = l4 flatMap (e => List.fill(e._2)(e._1)) toList
    val r5 = r4 flatMap (e => List.fill(e._2)(e._1)) toList

    op(plus(l5), plus(r5))
  }

  def eliminateBindings(phi: Bind): Expr = phi match {
    case Bind(quant, formals, body) => body match {
      case And(args) => quant(formals, And(eliminateBindingsFromNary(formals, args, false)))
      case Or(args) => quant(formals, Or(eliminateBindingsFromNary(formals, args, true)))
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
