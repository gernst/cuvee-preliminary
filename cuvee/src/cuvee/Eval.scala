package cuvee

case class Env(su: Map[Id, Expr], ty: Map[Id, Type]) {
  def contains(id: Id) = su contains id
  def apply(id: Id) = su apply id

  def eqs = {
    Eq.zip(su.toList)
  }

  def check(xs: Iterable[Id]) {
    for (x <- xs)
      ensure(su contains x, "undeclared variable", x, su.keySet)
  }

  def bind(fs: List[Formal]): Env = {
    val xs = fs map (_.id)
    val ts = fs map (_.typ)
    val re = Expr.id(xs)
    val xt = xs zip ts
    Env(su ++ re, ty ++ xt)
  }

  def assignUnchecked(xs: List[Id], es: List[Expr]): Env = {
    Env(su ++ (xs zip es), ty)
  }

  def assign(xs: List[Id], es: List[Expr]): Env = {
    check(xs)
    assignUnchecked(xs, es)
  }

  def havoc(xs: Iterable[Id]): (List[Formal], Env) = {
    check(xs)
    val re = Expr.fresh(xs)
    val formals = xs map (x => Formal(x rename re, ty(x)))
    val env = Env(su ++ re, ty)
    (formals.toList, env)
  }

  override def toString = {
    val strings = su map { case (x, e) => Pair(x, e) }
    strings.mkString("(", " ", ")")
  }
}

object Env {
  val empty = Env(Map.empty, Map.empty)
}

case class Path(fresh: List[Formal], path: List[Expr], env: Env) {
  def ::(phi: Expr) = {
    Path(fresh, phi :: path, env)
  }

  def bind(fs: List[Formal]) = {
    Path(fresh ++ fs, path, env)
  }

  def toExpr = {
    Exists(fresh, And(env.eqs ++ path))
  }
}

object Path {
  val empty = Path(List.empty, List.empty, Env.empty)
}

object Eval {
  var inferInvariants = false

  def eval(expr: Expr, st: State): Expr = {
    val old = Nil
    val env = st.env
    eval(expr, env, old, st)
  }

  def eval(let: Pair, env: Env, old: List[Env], st: State): (Id, Expr) = let match {
    case Pair(x, e) => (x, eval(e, env, old, st))
  }

  def eval(expr: Expr, env: Env, old: List[Env], st: State): Expr = expr match {
    case num: Num =>
      num

    case id: Id if (env contains id) =>
      env(id)

    case id: Id if (st.funs contains id) =>
      val (args, res) = st funs id
      ensure(args.isEmpty, "not constant", expr, env)
      id

    case id: Id =>
      error("unknown identifier", expr, env)

    case Old(inner) =>
      old match {
        case Nil =>
          error("no old state", expr, env)
        case env :: old =>
          eval(inner, env, old, st)
      }

    case Note(expr, attrs) =>
      val _expr = eval(expr, env, old, st)
      Note(_expr, attrs)

    case Eq(left, right) =>
      Eq(eval(left, env, old, st), eval(right, env, old, st))

    case Ite(test, left, right) =>
      Ite(eval(test, env, old, st), eval(left, env, old, st), eval(right, env, old, st))

    case Let(lets, body) =>
      val pairs = lets map (eval(_, env, old, st))
      val (xs, _es) = pairs.unzip
      eval(body, env assignUnchecked (xs, _es), old, st)

    case Select(array, index) =>
      Select(eval(array, env, old, st), eval(index, env, old, st))

    case Store(array, index, value) =>
      Store(eval(array, env, old, st), eval(index, env, old, st), eval(value, env, old, st))

    case Distinct(args) =>
      Distinct(args map (eval(_, env, old, st)))

    case And.nary(args) =>
      And(args map (eval(_, env, old, st)))

    case Or.nary(args) =>
      Or(args map (eval(_, env, old, st)))

    case App(id, args) if (st.funs contains id) =>
      val (types, res) = st funs id
      ensure(args.length == types.length, "wrong number of arguments", expr, env, st)
      App(id, args map (eval(_, env, old, st)))

    case App(id, args) =>
      error("unknown function", id, expr, env, st)

    case expr @ Bind(quant, formals, body) =>
      Bind(quant, formals, eval(body, env bind formals, old, st))

    case WP(prog, post) =>
      wp(List(prog), None, post, env, old, st)

    case Box(prog, post) =>
      box(List(prog), None, post, env, old, st)

    case Dia(prog, post) =>
      dia(List(prog), None, post, env, old, st)
  }

  def wp(progs: List[Prog], break: Option[Expr], post: Expr, env0: Env, old: List[Env], st: State): Expr = progs match {
    case Nil =>
      eval(post, env0, old, st)

    case Break :: rest =>
      break match {
        case Some(post) =>
          eval(post, env0, old, st)
        case None =>
          error("break not within while", break, env0, st)
      }

    case Block(progs, withOld) :: rest =>
      val old_ = if (withOld) env0 :: old else old
      wp(progs ++ rest, break, post, env0, old_, st)

    case Assign(lets) :: rest =>
      val pairs = lets map (eval(_, env0, old, st))
      val (xs, _es) = pairs.unzip
      val env1 = env0 assignUnchecked (xs, _es)
      wp(rest, break, post, env1, old, st)

    case Spec(xs, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc xs
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      _phi && Forall(formals, _psi ==> wp(rest, break, post, env1, old, st))

    case Choose(xs, phi) :: rest =>
      val (formals, env1) = env0 havoc xs
      val _phi = eval(phi, env1, env0 :: old, st)
      Exists(formals, _phi) && Forall(formals, _phi ==> wp(rest, break, post, env1, old, st))

    case If(test, left, right) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test ==> wp(left :: rest, break, post, env0, old, st)
      val _right = !_test ==> wp(right :: rest, break, post, env0, old, st)
      _left && _right

    case While(test, body, after, term, phi, psi) :: rest =>
      val mod = body.mod
      val mod_ = mod.toList

      val (formals, env1) = env0 havoc mod

      val _test = eval(test, env1, env1 :: old, st)
      val decrease = test ==> (0 <= term && term < Old(term))

      val hyp = Spec(mod_, decrease && phi, !test && psi)

      val _phi0 = eval(phi, env0, old, st)
      val _psi0 = eval(psi, env1, env0 :: old, st)

      val _phi1 = eval(phi, env1, old, st)
      val _psi1 = eval(psi, env1, env1 :: old, st)

      val use = _phi0 && Forall(formals, _psi0 ==> wp(rest, break, post, env1, old, st))
      val base = Forall(formals, (!_test && _phi1) ==> wp(List(after), break, psi, env1, env1 :: old, st))
      val step = Forall(formals, (_test && _phi1) ==> wp(List(body, hyp), Some(psi), psi, env1, env1 :: old, st))

      use && base && step

    case Call(name, in, out) :: rest if st.procdefs contains name =>
      val spec = contract(name, out, in, st)
      wp(spec :: rest, break, post, env0, old, st)

    case Call(name, _, _) :: rest =>
      error("unknown procedure", name)
  }

  def contract(name: Id, out: List[Id], in: List[Expr], st: State): Spec = {
    val Proc(xs, ys, pre, post, body) = st procdefs name

    ensure(in.length == xs.length, "wrong number of inputs", name, xs, in)
    ensure(out.length == ys.length, "wrong number of outputs", name, ys, out)

    val su1 = Expr.subst(xs, in)
    val su2 = Expr.subst(ys, out)
    val _pre = pre subst su1
    val _post = post subst (su1 ++ su2)

    Spec(out, _pre, _post)
  }

  def box(progs: List[Prog], break: Option[Expr], post: Expr, env0: Env, old: List[Env], st: State): Expr = progs match {
    case Nil =>
      eval(post, env0, old, st)

    case Break :: rest =>
      break match {
        case Some(post) =>
          eval(post, env0, old, st)
        case None =>
          error("break not within while", break, env0, st)
      }

    case Block(progs, withOld) :: rest =>
      val old_ = if (withOld) env0 :: old else old
      box(progs ++ rest, break, post, env0, old_, st)

    case Assign(lets) :: rest =>
      val pairs = lets map (eval(_, env0, old, st))
      val (xs, _es) = pairs.unzip
      val env1 = env0 assignUnchecked (xs, _es)
      box(rest, break, post, env1, old, st)

    case Spec(mod, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc mod
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      _phi && Forall(formals, _psi ==> box(rest, break, post, env1, old, st))

    case Choose(xs, phi) :: rest =>
      val (formals, env1) = env0 havoc xs
      val _phi = eval(phi, env1, env0 :: old, st)
      Exists(formals, _phi) && Forall(formals, _phi ==> box(rest, break, post, env1, old, st))

    case If(test, left, right) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test ==> box(left :: rest, break, post, env0, old, st)
      val _right = !_test ==> box(right :: rest, break, post, env0, old, st)
      _left && _right

    case While(test, body, Skip, term, phi, True) :: rest =>
      val mod = body.mod
      val mod_ = mod.toList

      var inv = phi

      val (formals, env1) = env0 havoc mod

      val _test1 = eval(test, env1, env1 :: old, st)

      if (inferInvariants) {
        val (others, env2) = env0 havoc mod
        // ⋀ s'. ¬ test s' ⟹ I s' ⟹ Q s s' ⟹ Q s0 s'
        val _test2 = eval(test, env2, env1 :: old, st)
        val _inv2 = eval(phi, env2, env1 :: old, st)
        // s0 == env0
        // s  == env1
        // s' == env2
        /* Forall(
            others,
          */
      }

      val _inv0 = eval(inv, env0, old, st)
      val _inv1 = eval(inv, env1, old, st)

      val init = Forall(formals, (!_test1 && _inv1) ==> box(rest, break, post, env1, env1 :: old, st))
      val step = Forall(formals, (_test1 && _inv1) ==> box(List(body), Some(inv), inv, env1, env1 :: old, st))

      _inv0 && init && step

    case While(test, body, after, term, phi, psi) :: rest =>
      val mod = body.mod
      val mod_ = mod.toList

      val (formals, env1) = env0 havoc mod

      val _test = eval(test, env1, env1 :: old, st)

      val hyp = Spec(mod_, phi, !test && psi)

      val _phi0 = eval(phi, env0, old, st)
      val _psi0 = eval(psi, env1, env0 :: old, st)

      val _phi1 = eval(phi, env1, old, st)
      val _psi1 = eval(psi, env1, env1 :: old, st)

      val use = _phi0 && Forall(formals, _psi0 ==> box(rest, break, post, env1, old, st))
      val base = Forall(formals, (!_test && _phi1) ==> box(List(after), break, psi, env1, env1 :: old, st))
      val step = Forall(formals, (_test && _phi1) ==> box(List(body, hyp), Some(psi), psi, env1, env1 :: old, st))

      use && base && step

    case Call(name, in, out) :: rest if st.procdefs contains name =>
      val spec = contract(name, out, in, st)
      box(spec :: rest, break, post, env0, old, st)

    case Call(name, _, _) :: rest =>
      error("unknown procedure", name)
  }

  def dia(progs: List[Prog], break: Option[Expr], post: Expr, env0: Env, old: List[Env], st: State): Expr = progs match {
    case Nil =>
      eval(post, env0, old, st)

    case Break :: rest =>
      break match {
        case Some(post) =>
          eval(post, env0, old, st)
        case None =>
          error("break not within while", break, env0, st)
      }

    case Block(progs, withOld) :: rest =>
      val old_ = if (withOld) env0 :: old else old
      dia(progs ++ rest, break, post, env0, old_, st)

    case Assign(lets) :: rest =>
      val pairs = lets map (eval(_, env0, old, st))
      val (xs, _es) = pairs.unzip
      val env1 = env0 assignUnchecked (xs, _es)
      dia(rest, break, post, env1, old, st)

    case Spec(mod, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc mod
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      _phi && Exists(formals, _psi && dia(rest, break, post, env1, old, st))

    case Choose(xs, phi) :: rest =>
      val (formals, env1) = env0 havoc xs
      val _phi = eval(phi, env1, env0 :: old, st)
      Exists(formals, _phi && dia(rest, break, post, env1, old, st))

    case If(test, left, right) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test && dia(left :: rest, break, post, env0, old, st)
      val _right = !_test && dia(right :: rest, break, post, env0, old, st)
      _left || _right

    case While(test, body, after, term, phi, psi) :: rest =>
      val mod = body.mod
      val mod_ = mod.toList

      val (formals, env1) = env0 havoc mod

      val _test = eval(test, env1, env1 :: old, st)
      val decrease = test ==> (0 <= term && term < Old(term))

      val hyp = Spec(mod_, decrease && phi, !test && psi)

      val _phi0 = eval(phi, env0, old, st)
      val _psi0 = eval(psi, env1, env0 :: old, st)

      val _phi1 = eval(phi, env1, old, st)
      val _psi1 = eval(psi, env1, env1 :: old, st)

      error("loop rule for dia likely incorrect")
      val use = _phi0 && Exists(formals, _psi0 && dia(rest, break, post, env1, old, st))
      val base = Forall(formals, (!_test && _phi1) ==> dia(List(after), break, psi, env1, env1 :: old, st))
      val step = Forall(formals, (_test && _phi1) ==> dia(List(body, hyp), Some(psi), psi, env1, env1 :: old, st))

      use && base && step

    case Call(name, in, out) :: rest if st.procdefs contains name =>
      val spec = contract(name, out, in, st)
      dia(spec :: rest, break, post, env0, old, st)

    case Call(name, _, _) :: rest =>
      error("unknown procedure", name)
  }

  def rel(body: Body, env0: Env, old: List[Env], st: State): List[Path] = {
    val Body(locals, progs) = body
    val env1 = env0 bind locals
    rel(progs, env1, old, st)
  }

  def rel(progs: List[Prog], env0: Env, old: List[Env], st: State): List[Path] = progs match {
    case Nil =>
      List(Path(List.empty, List.empty, env0))

    case Break :: rest =>
      error("break not within while", env0, st)

    case Block(progs, withOld) :: rest =>
      val old_ = if (withOld) env0 :: old else old
      rel(progs ++ rest, env0, old_, st)

    case Assign(lets) :: rest =>
      val pairs = lets map (eval(_, env0, old, st))
      val (xs, _es) = pairs.unzip
      val env1 = env0 assign (xs, _es)
      rel(rest, env1, old, st)

    case Spec(mod, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc mod
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      for (path <- rel(rest, env1, old, st))
        yield _phi :: _psi :: path bind formals

    case If(test, left, right) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = for (path <- rel(left :: rest, env0, old, st))
        yield _test :: path
      val _right = for (path <- rel(right :: rest, env0, old, st))
        yield !_test :: path
      _left ++ _right

    case While(test, body, after, term, phi, psi) :: rest =>
      val mod = body.mod ++ after.mod
      val mod_ = mod.toList
      val spec = Spec(mod_, phi, !test && psi)
      rel(spec :: rest, env0, old, st)

    case Call(name, in, out) :: rest if st.procdefs contains name =>
      val spec = contract(name, out, in, st)
      rel(spec :: rest, env0, old, st)

    case Call(name, _, _) :: rest =>
      error("unknown procedure", name)
  }

  /**
   * Determine all paths through proc (splitting conditionals)
   *  wrt. state parameters ps, such that the path takes a transition from xs0 to xs1.
   *  Return the instantiated precondition as well as the paths,
   *  which store constraints and variable assignments for xs1 in the successor states.
   */
  def forward(proc: Proc, ps: List[Formal], in: List[Formal], out: List[Formal], init: List[Expr], st: State): List[(Expr, Path)] = {
    val xs: List[Id] = ps
    val (pre, post, body) = proc call (ps, xs, in, out)

    val env0 = Env.empty
    val env1 = env0 bind (ps ++ in ++ out)
    val env2 = env1.assign(xs, init)
    val old = Nil
    val _pre = Eval.eval(pre, env2, old, st)
    val paths = Eval.rel(body, env2, old, st)

    for (path <- paths)
      yield (_pre, path)
  }
}