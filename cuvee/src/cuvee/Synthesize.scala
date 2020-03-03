package cuvee

sealed trait Recipe

object Recipe {
  case object auto extends Recipe
  case object output extends Recipe
}

case class Step(op: Id, ps: List[Formal], in: List[Formal], out: List[Formal], ex: List[Formal], path: List[Expr], su: Map[Id, Expr]) {
  /** Formula such that post is checked to hold in the post-state */
  def ==>(post: Expr) = {
    Forall(
      ps ++ in ++ out ++ ex,
      And(path) ==> (post subst su))
  }

  /** Formula such that the step is taken and the given postcondition holds */
  def &&(post: Expr) = {
    Exists(
      ps ++ in ++ out ++ ex,
      And(path) && (post subst su))
  }

  /** Cormula to check that ps(pos) is guaranteed to be equal to arg in the post-state */
  def isConsumer(vs: List[Formal], pos: Int, arg: Expr) = {
    val Formal(x, _) = ps(pos)
    val post = x === arg

    val phi = Forall(
      vs ++ in ++ out ++ ex,
      And(path) ==> (post subst su))
    phi
  }
}

case class Synthesize(A: Obj, C: Obj, R: Id, state: State, solver: Solver) {
  /** state formal parameters */
  val as = A.state
  val cs = C.state
  val as_ = as map (_.prime)
  val cs_ = cs map (_.prime)

  /** corresponding state variables */
  val ax: List[Id] = as
  val ax_ : List[Id] = as_
  val cx: List[Id] = cs
  val cx_ : List[Id] = cs_

  ensure(
    A.state.toSet disjoint C.state.toSet,
    "overlapping state variables", A, C)

  def steps(o: Obj, es: List[Expr]) = {
    import Eval.forward

    val Obj(ps, init, ops) = o

    for (
      (op, proc) <- ((Id.init, init) :: ops);
      in = proc.in; out = proc.out;
      (pre, Path(ex, path, env)) <- forward(proc, ps, in, out, es, state)
    ) yield {
      Step(op, ps, in, out, ex, pre :: path, env.su)
    }
  }

  def apply(recipe: Recipe): List[Expr] = recipe match {
    case Recipe.auto =>
      val inds = inductivePositions(as)
      val List((sort, dt, pos)) = inds
      val defs = fromConsumer(sort, dt, pos)
      defs

    case Recipe.output =>
      fromOutput()
  }

  def fromOutput(): List[Expr] = {
    for ((aop, ap) <- A.ops.filterNot(proc => proc._2.out.isEmpty)) yield {
      val cp = C op aop
      ensure((ap.in ++ ap.out).map(_.typ) == (cp.in ++ cp.out).map(_.typ))

      val aBody = step(ap, as, ax, ax_, ap.in, ap.out)
      val insEq = cp.in.prime.zip(ap.in.ids).map(p => p._1.id -> p._2).toMap
      val cBody = step(cp, cs, cx, cx_, cp.in.prime, cp.out.prime) subst insEq
      val outsEq: Expr = And(ap.out.ids.zip(cp.out.prime.ids).map(p => Eq(p._1, p._2)))
      define(as ++ cs, App(R, as ++ cs), Forall(as_ ++ cs_ ++ ap.in ++ ap.out ++ cp.in.prime ++ cp.out.prime, (aBody && cBody) ==> outsEq))
    }
  }

  def define(bound: List[Formal], lhs: Expr, rhs: Expr): Expr = {
    val simplify = Simplify(solver)
    solver.scoped {
      solver.bind(bound)
      val _rhs = simplify(rhs)
      Forall(bound, lhs === _rhs)
    }
  }

  def fromConsumer(sort: Sort, dt: Datatype, pos: Int): List[Expr] = {
    val ai = ax(pos)

    for ((constr, args, hyps) <- dt.induction(ai, sort)) yield {
      val vs: List[Id] = args
      val pat = Apps(constr :: vs)

      val as0 = as patch (pos, args, 1)
      val ax0 = ax updated (pos, pat)
      val lhs = App(R, ax0 ++ cx)

      if (hyps.isEmpty) {
        define(as0 ++ cs, lhs, base(ax, cx, ax0, cx))
      } else {
        val cases = for (hyp <- hyps) yield {
          rec(as0, pos, vs(hyp), ax0, cx, ax_, cx_)
        }

        define(as0 ++ cs, lhs, And(cases))
      }
    }
  }

  /** Synthesize base case from transitions (this is like a producer) */
  def base(as0: List[Expr], cs0: List[Expr], as1: List[Expr], cs1: List[Expr]): Expr = {
    lockstep(
      A.init, as0, as1,
      C.init, cs0, cs1)
  }

  /** Synthesize recursive cases from transitions that consume as0(pos) such that as1(pos) == arg */
  def rec(bound: List[Formal], pos: Int, arg: Id, as0: List[Expr], cs0: List[Expr], as1: List[Id], cs1: List[Id]): Expr = {
    val asteps = steps(A, as0)
    val csteps = steps(C, cs0)

    val alts = for (
      astep @ Step(op, as, ain, aout, aex, apath, asu) <- asteps if solver isTrue astep.isConsumer(bound, pos, arg);
      cstep @ Step(`op`, cs, cin, cout, cex, cpath, csu) <- csteps
    ) yield {
      assert(aex.toSet disjoint cex.toSet)

      val ex = aex ++ cex
      val path = apath ++ cpath
      val ins = Eq.zip(ain, cin)
      val outs = Eq.zip(aout map (_ subst asu), cout map (_ subst csu))
      val psi = App(R, (as map (_ subst asu)) ++ (cs map (_ subst csu)))
      val constrs = ins ++ path ++ outs ++ List(psi)

      import Simplify.and
      import Simplify.norm
      val phi = and(norm(constrs))
      Exists(ex, phi)
    }

    /* val post = as1(pos) === arg

    val alts = for (
      (id, aproc) <- A.ops;
      phi = Forall(bound, step(aproc, as, as0, as1) ==> post) if solver.isTrue(phi);
      cproc = C.op(id)
    ) yield {
      val phi = lockstep(
        aproc, as0, as1,
        cproc, cs0, cs1)

      val psi = App(R, as1 ++ cs1)

      phi && psi
    } */

    Or(alts)
  }

  /**
   * Determine positions of abstract state variables
   * which admit induction over an ADT.
   */
  def inductivePositions(xs: List[Formal]) = {
    for ((Formal(_, sort: Sort), i) <- xs.zipWithIndex if state.datatypes contains sort) yield {
      val dt = state datatypes sort
      (sort, dt, i)
    }
  }

  /**
   * Determine all paths through proc (splitting conditionals)
   *  wrt. state parameters ps, such that the path takes a transition from xs0 to xs1.
   *  Return the instantiated precondition as well as the paths,
   *  which store constraints and variable assignments for xs1 in the successor states.
   */
  def paths(proc: Proc, st: List[Formal], xs0: List[Expr], xs1: List[Expr],
    in: List[Formal], out: List[Formal]) = {
    val xs: List[Id] = st
    val (pre, post, body) = proc call (st, xs, in, out)
    val env0 = Env.empty
    val env1 = env0 bind (st ++ in ++ out)
    val env2 = env1.assign(xs, xs0)
    val old = Nil
    val _pre = Eval.eval(pre, env2, old, state)
    val paths = Eval.rel(body, env2, old, state)
    val conds = for (Path(fresh, path, env) <- paths) yield {
      val eqs = for ((x, x1) <- (xs zip xs1)) yield {
        env(x) === x1
      }

      val outs = for (o <- out) yield {
        o.id === env(o.id)
      }

      Exists(fresh, And(eqs ++ outs ++ path))
    }
    (_pre, conds)
  }

  def step(proc: Proc, st: List[Formal], xs0: List[Expr], xs1: List[Expr],
    in: List[Formal], out: List[Formal]): Expr = {
    val (_pre, _paths) = paths(proc, st, xs0, xs1, in, out)
    _pre && Or(_paths)
  }

  def lockstep(
    aproc: Proc, as0: List[Expr], as1: List[Expr],
    cproc: Proc, cs0: List[Expr], cs1: List[Expr],
    in: List[Formal], out: List[Formal]): Expr = {
    val astep = step(aproc, as, as0, as1, in, out)
    val cstep = step(cproc, cs, cs0, cs1, in, out)
    astep && cstep
  }

  def step(proc: Proc, st: List[Formal], xs0: List[Expr], xs1: List[Expr]): Expr = {
    val in = proc.in
    val out = proc.out
    Exists(
      in ++ out,
      step(proc, st, xs0, xs1, in, out))
  }

  def lockstep(
    aproc: Proc, as0: List[Expr], as1: List[Expr],
    cproc: Proc, cs0: List[Expr], cs1: List[Expr]): Expr = {
    val in = aproc.in
    val out = aproc.out
    Exists(
      in ++ out,
      lockstep(
        aproc, as0, as1,
        cproc, cs0, cs1, in, out))
  }
}