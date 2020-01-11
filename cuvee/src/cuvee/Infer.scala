package cuvee

object Infer {
  import Id._
  import Sort._
  import Type._

  val elem = Sort("Elem")
  val list = Sort("List")

  val a = Id("a")
  val R = Id("R")

  val data = Datatype(List(), List(
    Constr(Id("nil"), List()),
    Constr(Id("cons"), List(Sel(head, elem), Sel(tail, list)))))

  val stack = Id("stack")
  val index = Id("index")
  val values = Id("values")

  object ListStack extends Obj(
    List(Formal(stack, list)),
    Proc(List(), List(), True,
      Assign(List(Pair(stack, nil)))),
    List(
      "push" -> Proc(List(Formal(a, elem)), List(), True,
        Assign(List(
          Pair(stack, a :: stack)))),
      "pop" -> Proc(List(), List(Formal(a, elem)), stack !== nil,
        Assign(List(
          Pair(stack, stack.tail),
          Pair(a, stack.head))))))

  object ArrayStack extends Obj(
    List(Formal(index, int), Formal(values, array(int, elem))),
    Proc(List(), List(), True,
      Assign(List(Pair(index, 0)))),
    List(
      "push" -> Proc(List(Formal(a, elem)), List(), True,
        Assign(List(
          Pair(index, index + 1),
          Pair(values, values store (index, a))))),
      "pop" -> Proc(List(), List(Formal(a, elem)), index > 0,
        Assign(List(
          Pair(index, index - 1),
          Pair(a, values select (index - 1)))))))

  def main(args: Array[String]) {
    var st = State.default
    st = st declare (list, 0, data)
    val infer = new Infer(ListStack, ArrayStack, R, st)
    val defn = infer.induct(list, data, 0)
    println(defn)
  }
}

case class Infer(A: Obj, C: Obj, R: Id, st: State) {
  val as = A.state
  val cs = C.state
  val as_ = as map (_.prime)
  val cs_ = cs map (_.prime)
  val ainit = A.init
  val cinit = C.init

  ensure(
    A.state.toSet disjoint C.state.toSet,
    "overlapping state variables", A, C)

  def infer(as: List[Pat], cs: List[Pat], ctx: List[Expr]): List[Expr] = {
    ???
  }

  def step(proc: Proc, ps: List[Formal], xs: List[Id]): List[Expr] = {
    val (xi, xo, pre, prog) = proc call (ps, xs)
    val paths = Eval.rel(prog, ps ++ xi ++ xo, st)
    List(pre, Or(paths map (_.toExpr)))
  }

  def step(proc: Proc, ps: List[Formal], xs0: List[Expr], xs1: List[Id]): List[Expr] = {
    val (xi, xo, pre, prog) = proc call (ps, xs1)
    val su = Expr.subst(xs1, xs0)
    val ty = ps map (_.typ)
    val env0 = Env(su, Map(xs1 zip ty: _*))
    val env1 = env0 bind (xi ++ xo)
    val paths = Eval.rel(List(prog), env1, List(), st)
    List(pre, Or(paths map (_.toExpr)))
  }

  def lockstep(
    aproc: Proc, as0: List[Expr], as1: List[Id],
    cproc: Proc, cs0: List[Expr], cs1: List[Id]) = {
    val aphi = step(aproc, as, as0, as1)
    val cphi = step(aproc, as, as0, as1)
    aphi ++ cphi
  }

  /**
   * Find constraints
   *  - from abstract transitions as0 -> as1 and as1 -> as0
   *  - from the corresponding concrete transitions, where cs0 is given
   *  - specifically, add some R(as1, cs1) for a newly found cs1
   */
  def recurse(as0: List[Expr], cs0: List[Id], as1: List[Id], ctx: List[Expr]): List[Expr] = {
    val cs1 = cs0 map (_.prime)
    val rec = App(R, as1 ++ cs1)
    val ops = for (((_, aproc), (_, cproc)) <- A.ops zip C.ops) yield {
      And(lockstep(aproc, as0, as1, cproc, cs0, cs1))
    }
    List(rec, Or(ops))
  }

  def base(fun: Id, pos: Int, as0: List[Id], cs0: List[Id], ctx: List[Expr]): List[Expr] = {
    step(cinit, cs, cs0)
  }

  def recurse(fun: Id, args: List[Id], pos: Int, hyp: List[Int], as0: List[Id], cs0: List[Id], ctx: List[Expr]): List[Expr] = {
    for (i <- hyp) yield {
      // a1 is the argument of the constructor for which a recursive call should be generated
      val a1 = args(i)
      val as1 = as0 updated (pos, a1)
      val phis = recurse(as0, cs0, as1, ctx)
      And(phis)
    }
  }

  /**
   * Introduce a recursive definition by induction
   *  @param sort the name of the data type used for the induction scheme
   *  @param data the constructor definitions
   *  @param pos is the position of the argument used for induction
   *  @param as, cs the list of state variables under consideration
   *  @param ctx the current context
   */
  def induct(sort: Sort, data: Datatype, pos: Int, as: List[Id], cs: List[Id], ctx: List[Expr]): Expr = {
    val Datatype(params, constrs) = data
    ensure(pos < as.length, "unsupported: induction over concrete state", pos, as, cs)
    ensure(params.isEmpty, "unsupported: induction over parametric data types", sort, data)

    val arg = as(pos)

    val cases = for (constr <- constrs) yield {
      induct(sort, constr, pos, as, cs, ctx)
    }

    Match(arg, cases)
  }

  def induct(sort: Sort, data: Datatype, pos: Int): Expr = {
    val rhs = induct(sort, data, pos, as, cs, Nil)
    App(R, as ++ cs) === rhs
  }

  /**
   * A single case for a given constructor
   */
  def induct(sort: Sort, constr: Constr, pos: Int, as: List[Id], cs: List[Id], ctx: List[Expr]): Case = {
    val Constr(id, sels) = constr

    // fresh variables for each constructor argument,
    // named as the selectors
    val args = for (sel <- sels)
      yield Expr.fresh(sel.id)

    // recursive positions of constructor arguments
    // for which an inductive hypothesis in the form of a recursive call is generated
    val hyp = for ((sel, i) <- sels.zipWithIndex if sel.typ == sort)
      yield i

    // synthesize constraints for this case
    val phis = if (args.isEmpty) {
      base(id, pos, as, cs, ctx)
    } else {
      recurse(id, args, pos, hyp, as, cs, ctx)
    }

    Case(UnApps(id :: args), And(phis))
  }
}