package cuvee

case class Env(su: Map[Id, Expr], ty: Map[Id, Type]) {
  def contains(id: Id) = su contains id
  def apply(id: Id) = su apply id

  def check(xs: Iterable[Id]) {
    for (x <- xs)
      ensure(su contains x, "undeclared program variable", x, su.keySet)
  }

  def bind(fs: List[Formal]): Env = {
    val xs = fs map (_.id)
    val ts = fs map (_.typ)
    val re = Expr.id(xs)
    val xt = xs zip ts
    Env(su ++ re, ty ++ xt)
  }

  def assign(xs: List[Id], es: List[Expr]): Env = {
    check(xs)
    Env(su ++ (xs zip es), ty)
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

object Eval {
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

    case Eq(left, right) =>
      Eq(eval(left, env, old, st), eval(right, env, old, st))

    case Ite(test, left, right) =>
      Ite(eval(test, env, old, st), eval(left, env, old, st), eval(right, env, old, st))

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
      val env1 = env0 assign (xs, _es)
      wp(rest, break, post, env1, old, st)

    case Spec(xs, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc xs
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      _phi && Forall(formals, _psi ==> wp(rest, break, post, env1, old, st))

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
      throw Error(s"Call to unknown procedure $name")
  }

  def contract(name: Id, out: List[Id], in: List[Expr], st: State): Spec = {
    val DefineProc(_, xs, ys, _, pre, post) = st procdefs name

    if (in.size != xs.size) {
      throw Error(s"Call to procedure $name requires ${xs.size} arguments but ${in.size} were given");
    }
    if (out.size != ys.size) {
      throw Error(s"Call to procedure $name requires ${ys.size} return values but ${out.size} were given");
    }

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
      val env1 = env0 assign (xs, _es)
      box(rest, break, post, env1, old, st)

    case Spec(mod, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc mod
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      _phi ==> Forall(formals, _psi ==> box(rest, break, post, env0, old, st))

    case If(test, left, right) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test ==> box(left :: rest, break, post, env0, old, st)
      val _right = !_test ==> box(right :: rest, break, post, env0, old, st)
      _left && _right

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

      val use = _phi0 ==> Forall(formals, _psi0 ==> box(rest, break, post, env1, old, st))
      val base = Forall(formals, (!_test && _phi1) ==> box(List(after), break, psi, env1, env1 :: old, st))
      val step = Forall(formals, (_test && _phi1) ==> box(List(body, hyp), Some(psi), psi, env1, env1 :: old, st))

      use && base && step

    case Call(name, in, out) :: rest if st.procdefs contains name =>
      val spec = contract(name, out, in, st)
      box(spec :: rest, break, post, env0, old, st)

    case Call(name, _, _) :: rest =>
      throw Error(s"Call to unknown procedure $name")
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
      val env1 = env0 assign (xs, _es)
      dia(rest, break, post, env1, old, st)

    case Spec(mod, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc mod
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      _phi && Exists(formals, _psi && dia(rest, break, post, env0, old, st))

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

      val use = _phi0 && Exists(formals, _psi0 && dia(rest, break, post, env1, old, st))
      val base = Forall(formals, (!_test && _phi1) ==> dia(List(after), break, psi, env1, env1 :: old, st))
      val step = Forall(formals, (_test && _phi1) ==> dia(List(body, hyp), Some(psi), psi, env1, env1 :: old, st))

      use && base && step

    case Call(name, in, out) :: rest if st.procdefs contains name =>
      val spec = contract(name, out, in, st)
      dia(spec :: rest, break, post, env0, old, st)

    case Call(name, _, _) :: rest =>
      throw Error(s"Call to unknown procedure $name")
  }
}