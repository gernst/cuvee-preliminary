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
    val infer = new Infer(ListStack, ArrayStack, R)
    val defn = infer.induct(list, data, 0)
    println(defn)
  }
}

case class Infer(A: Obj, C: Obj, R: Id) {
  val as: List[Id] = A.state
  val cs: List[Id] = C.state
  val as_ = as map (_.prime)
  val cs_ = cs map (_.prime)

  ensure(
    A.state.toSet disjoint C.state.toSet,
    "overlapping state variables", A, C)

  def infer(as: List[Pat], cs: List[Pat], ctx: List[Expr]): List[Expr] = {
    ???
  }

  def step(proc: Proc, ps: List[Formal], s0: List[Id], s1: List[Id]) = {
    val xi = proc.in
    val xo = proc.out
    proc call (ps, s0, xi, xo)
  }

  def lockstep(
    ap: Proc, as0: List[Id], as1: List[Id],
    cp: Proc, cs0: List[Id], cs1: List[Id]) = {

    val Proc(ai, ao, apre, abody) = ap
    val Proc(ci, co, cpre, cbody) = cp

    val re = Expr.subst[Id](ai ++ ao, ci ++ co)
    val cbody_ = cbody replace re
  }

  /**
   * Find constraints
   *  - from abstract transitions as0 -> as1 and as1 -> as0
   *  - from the corresponding concrete transitions, where cs0 is given
   *  - specifically, add some R(as1, cs1) for a newly found cs1
   */
  def recurse(as0: List[Pat], cs0: List[Id], as1: List[Pat], ctx: List[Expr]): List[Expr] = {
    val cs1 = cs0 map (_.prime)
    val rec = App(R, as1 ++ cs1)
    List(rec)
  }

  /**
   * Synthesize recursive calls with constraints
   *  @param a0 == as0(pos)
   */
  def recurse(a0: Pat, pos: Int, hyp: List[Int], as0: List[Pat], cs0: List[Id], ctx: List[Expr]): List[Expr] = a0 match {
    case _: Id =>
      // No recursive invocation
      val Proc(Nil, Nil, True, prog) = C.init
      val base = Dia(prog, True)
      List(base)
    case UnApp(fun, args) =>
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

    val pat = UnApps(id :: args)
    val as_ = as updated (pos, pat)

    // synthesize constraints for this case
    val phis = recurse(pat, pos, hyp, as_, cs, ctx)
    val expr = And(phis)

    Case(pat, expr)
  }
}