package tacas2020.pure

import tacas2020.Error

sealed trait Rule

case class Forward(expr: Pure, phi: Pure, cond: Option[Pure] = None) extends Rule {
  // e.g. head(xs) ~> exists y,ys. xs = cons(y, ys) if xs != nil
  //      which then hopefully triggers rewrites

  // Note: applies to terms that occur not necessaraily at the top-level
}

case class Rewrite(lhs: Pure, rhs: Pure, cond: Option[Pure] = None) extends Rule {
  // e.g. f[x:=y][x] == y
  //      n != 0 ==> (m * n) / n = m
}

case class Weaken(pre: Pure, concl: Pure, cond: Option[Pure] = None) extends Rule {
  // e.g. f[x:=y] == g ~> g[x] == y
}

case class Case(pre: Pure, concl: App) {
  def bound = pre.free -- concl.free

  def apply(su: Subst) = {
    val _pre = Bind.ex(bound, pre)
    _pre subst su
  }
}

case class Inductive(pred: Fun, cases: List[Case]) extends Rule {
  for (Case(pre, App(fun, args)) <- cases) {
    assert(fun == pred, "ill-defined: " + this)
  }
  // e.g. even(0); even(n) ==> even(n + 2)
}

object Inductive {
  def apply(pred: Fun, cases: Case*): Inductive = {
    Inductive(pred, cases.toList)
  }
}

object Simplify {
  val default = Simplify(Nil)

  def simplify(expr: Pure, rw: List[Rule]): Pure = {
    val self = Simplify(rw)
    self(expr)
  }

  def simplify(expr: Pure, path: List[Pure], rw: List[Rule]): Pure = {
    val self = Simplify(rw)
    self(expr, path)
  }
}

case class Context(path: List[Pure], eqs: Subst) {
  def ::(phi: Pure) = {
    Context(phi :: path, eqs)
  }

  def +(xe: (Var, Pure)) = {
    val (x, e) = xe
    assert(!(e.free contains x), "recursive equation: " + (x === e))
    Context(path, eqs + xe)
  }

  def maps(x: Var) = {
    eqs contains x
  }

  def apply(x: Var) = {
    eqs(x)
  }

  def contains(expr: Pure) = {
    path contains expr
  }

  override def toString = (path ++ eqs).mkString(" && ")
}

case class Goal(ctx: Context, suc: Pure) {
  override def toString = ctx + " ==> " + suc
}

object Context {
  val empty = Context(List.empty, Subst.empty)
  def from(path: List[Pure]) = Context(path, Subst.empty)
}

case class Simplify(rw: List[Rule]) extends (Pure => Pure) {
  def apply(phi: Pure, ctx: Context): Pure = {
    try {
      simplify(phi, ctx)
    } catch {
      case _: StackOverflowError =>
        throw Error("nonterminating simplifcation", phi)
    }

  }

  def apply(phi: Pure, path: List[Pure]): Pure = {
    apply(phi, Context.from(path))
  }

  def apply(phi: Pure): Pure = {
    apply(phi, Context.empty)
  }

  def simplify(exprs: List[Pure], ctx: Context): List[Pure] = {
    exprs map (simplify(_, ctx))
  }

  def rewrite(exprs: List[Pure], ctx: Context): List[Pure] = {
    exprs map (rewrite(_, ctx))
  }

  def simplify(phi: Pure, ctx: Context): Pure = {
    val res = _simplify(phi, ctx)
    // println(phi + " ~> " + res)
    if (res == phi) phi // re-use of objects
    else res
  }

  def rewrite(phi: Pure, ctx: Context): Pure = {
    val res = _rewrite(phi, ctx)
    // println(phi + " ~> " + res)
    if (res == phi) phi // re-use of objects
    else res
  }

  def _simplify(phi: Pure, ctx: Context): Pure = phi match {
    case Pure.not(phi) =>
      !simplify(phi, ctx)

    case Pure.and(phi, psi) =>
      val (_phi, _psi) = binary(phi, true, psi, true, ctx)
      _phi && _psi

    case Pure.or(phi, psi) =>
      val (_phi, _psi) = binary(phi, false, psi, false, ctx)
      _phi || _psi

    case Pure.imp(phi, Pure.imp(psi, chi)) =>
      simplify((phi && psi) ==> chi, ctx)

    case Pure.imp(phi, psi) =>
      val (_phi, _psi) = binary(phi, true, psi, false, ctx)
      _phi ==> _psi

    case Pure._eq(left, right) =>
      val _left = simplify(left, ctx)
      val _right = simplify(right, ctx)
      literal(_left === _right, ctx)

    case Bind(q, xs, body) =>
      val _body = simplify(body, ctx) // XXX: assumes that ctx does not extend over free variables of body
      val __body = prune(_body, q, xs, true)
      q(xs, __body)

    case _ if phi.typ == Sort.bool =>
      literal(phi, ctx)

    case _ =>
      rewrite(phi, ctx)
  }

  def rewrite(expr: App, ctx: Context, rw: List[Rule]): Pure = (expr, rw) match {
    case (_, Nil) =>
      expr

    case (_, Rewrite(pat, rhs, cond) :: rest) =>
      bind(pat, expr, Subst.empty) match {
        case None =>
          rewrite(expr, ctx, rest)
        case Some(env) =>
          cond match {
            case None =>
              simplify(rhs subst env, ctx)
            case Some(phi) =>
              val _phi = simplify(phi subst env, ctx)
              if (_phi == True) {
                val res = rhs subst env
                println(expr + " ~> " + res)
                simplify(res, ctx)
              } else {
                rewrite(expr, ctx, rest)
              }
          }
      }

    case (App(fun, args), Inductive(pred, cases) :: rest) if fun == pred =>
      val matches = cases flatMap {
        case cs @ Case(pre, pat) =>
          val su = bind(pat, expr, Subst.empty)
          su map cs.apply
      }

      if (matches.isEmpty)
        rewrite(expr, ctx, rest)
      else
        Pure.or(matches)

    case (_, _ :: rest) =>
      rewrite(expr, ctx, rest)
  }

  def _rewrite(expr: Pure, ctx: Context): Pure = expr match {
    case x: Var if ctx maps x =>
      rewrite(ctx(x), ctx)

    case x: Var =>
      x

    case App(fun, args) =>
      val _args = rewrite(args, ctx)
      val _expr = App(fun, _args)
      rewrite(_expr, ctx, rw)

    case Bind(q, xs, body) => // XXX: assumes that ctx does not extend over free variables of body
      Bind(q, xs, simplify(body, ctx))
  }

  def literal(phi: Pure, ctx: Context): Pure = {
    Predef.assert(phi.typ == Sort.bool)
    if (ctx contains False) True
    else if (ctx contains phi) True
    else if (ctx contains !phi) False
    else rewrite(phi, ctx)
  }

  def binary(
    phi: Pure, phi_pos: Boolean,
    psi: Pure, psi_pos: Boolean,
    ctx: Context,
    psi_done: Boolean = false,
    swap: Boolean = false): (Pure, Pure) =
    {
      val newctx = if (psi_pos) assume(psi, ctx) else assert(psi, ctx)
      val newphi = simplify(phi, newctx)
      val phi_done = phi == newphi

      if (phi_done && psi_done) {
        if (swap) (psi, phi)
        else (phi, psi)
      } else {
        binary(
          psi, psi_pos,
          newphi, phi_pos,
          ctx,
          phi_done, !swap)
      }
    }

  def assume(phi: Pure, ctx: Context): Context = phi match {
    case True =>
      ctx
    case Pure.not(psi) =>
      assert(psi, ctx)
    case Pure.and(phi, psi) =>
      assume(phi, assume(psi, ctx))
    case Pure._eq(x: Var, e) if !(e.free contains x) =>
      ctx + (x -> e)
    case Pure._eq(e, x: Var) if !(e.free contains x) =>
      assume(x === e, ctx)
    case _ =>
      phi :: ctx
  }

  def assert(phi: Pure, ctx: Context): Context = phi match {
    case False =>
      ctx
    case Pure.not(psi) =>
      assume(psi, ctx)
    case Pure.imp(phi, psi) =>
      assert(phi, assume(psi, ctx))
    case Pure.or(phi, psi) =>
      assert(phi, assert(psi, ctx))
    case _ =>
      !phi :: ctx
  }

  def assume(args: List[Pure], ctx: Context): Context = {
    args.foldRight(ctx)(assume)
  }

  def assert(args: List[Pure], ctx: Context): Context = {
    args.foldRight(ctx)(assert)
  }

  def prune(phi: Pure, q: Quant, bound: Set[Var], pos: Boolean): Pure = phi match {
    case Pure._eq(x: Var, e) if !(e.free contains x) && (bound contains x) =>
      if (pos && q == Bind.ex || !pos && q == Bind.all) {
        True
      } else {
        phi
      }
    case Pure._eq(e, x: Var) if !(e.free contains x) =>
      prune(x === e, q, bound, pos)
    case Pure.not(psi) =>
      val _psi = prune(psi, q, bound, !pos)
      !_psi
    case Pure.imp(phi, psi) =>
      val _phi = prune(phi, q, bound, !pos)
      val _psi = prune(psi, q, bound, pos)
      _phi ==> _psi
    case Pure.or(phi, psi) =>
      val _phi = prune(phi, q, bound, !pos)
      val _psi = prune(psi, q, bound, !pos)
      _phi || _psi
    case Pure.and(phi, psi) =>
      val _phi = prune(phi, q, bound, pos)
      val _psi = prune(psi, q, bound, pos)
      _phi && _psi
    case _ =>
      phi
  }

  def bind(pats: List[Pure], args: List[Pure], env0: Subst): Option[Subst] = (pats, args) match {
    case (Nil, Nil) =>
      Some(env0)
    case (pat :: pats, arg :: args) =>
      for (
        env1 <- bind(pat, arg, env0);
        env2 <- bind(pats, args, env1)
      ) yield env2
    case _ =>
      None
  }

  def bind(pat: Pure, arg: Pure, env: Subst): Option[Subst] = (pat, arg) match {
    case (x: Var, _) if !(env contains x) =>
      Some(env + (x -> arg))
    case (x: Var, _) if (env contains x) && (env(x) == arg) =>
      Some(env + (x -> arg))
    case (x: Var, _) =>
      None
    case (App(fun1, pats), App(fun2, args)) if fun1 == fun2 =>
      bind(pats, args, env)
    case _ =>
      None
  }
}