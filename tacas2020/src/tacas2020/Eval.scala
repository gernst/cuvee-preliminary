package tacas2020

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
    val strings = su map { case (x, e) => Let(x, e) }
    strings.mkString("(", " ", ")")
  }
}

object Env {
  val empty = Env(Map.empty, Map.empty)
}

object Eval {
  def eval(let: Let, env: Env, old: List[Env], st: State): (Id, Expr) = let match {
    case Let(x, e) => (x, eval(e, env, old, st))
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

    case App(id, args) if (st.funs contains id) =>
      val (types, res) = st funs id
      ensure(args.length == types.length, "wrong number of arguments", expr, env, st)
      App(id, args map (eval(_, env, old, st)))

    case App(id, args) =>
      error("unknown function", expr, env, st)

    case expr @ Bind(quant, formals, body) =>
      Bind(quant, formals, eval(body, env bind formals, old, st))

    case WP(prog, post) =>
      wp(List(prog), post, env, old, st)

    case Box(prog, post) =>
      box(List(prog), post, env, old, st)

    case Dia(prog, post) =>
      dia(List(prog), post, env, old, st)
  }

  def wp(progs: List[Prog], post: Expr, env0: Env, old: List[Env], st: State): Expr = progs match {
    case Nil =>
      val _psi = eval(post, env0, old, st)
      _psi

    case Block(progs) :: rest =>
      wp(progs ++ rest, post, env0, old, st)

    case Assign(lets) :: rest =>
      val pairs = lets map (eval(_, env0, old, st))
      val (xs, _es) = pairs unzip
      val env1 = env0 assign (xs, _es)
      wp(rest, post, env1, old, st)

    case Spec(xs, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc xs
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      _phi && Forall(formals, _psi ==> wp(rest, post, env1, old, st))

    case If(test, left, right) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test ==> wp(left :: rest, post, env0, old, st)
      val _right = !_test ==> wp(right :: rest, post, env0, old, st)
      _left && _right

    case While(test, body, term, phi, psi) :: rest =>
      val mod = body.mod
      val mod_ = mod.toList

      val (formals, env1) = env0 havoc mod

      val _test = eval(test, env1, env1 :: old, st)
      val decrease = test ==> (0 <= term && term < Old(term))

      val hyp = Spec(mod_, decrease && phi, !test && psi)

      val _phi0 = eval(phi, env0, old, st)
      val _phi1 = eval(phi, env1, old, st)
      val _psi1 = eval(psi, env1, env1 :: old, st)

      val use = _phi0 && Forall(formals, _psi1 ==> wp(rest, post, env1, old, st))
      val base = Forall(formals, (!_test && _phi1) ==> _psi1)
      val step = Forall(formals, (_test && _phi1) ==> wp(List(body, hyp), psi, env1, env1 :: old, st))

      use && base && step
  }

  def box(progs: List[Prog], post: Expr, env0: Env, old: List[Env], st: State): Expr = progs match {
    case Nil =>
      val _psi = eval(post, env0, old, st)
      _psi

    case Block(progs) :: rest =>
      box(progs ++ rest, post, env0, old, st)

    case Assign(lets) :: rest =>
      val pairs = lets map (eval(_, env0, old, st))
      val (xs, _es) = pairs unzip
      val env1 = env0 assign (xs, _es)
      box(rest, post, env1, old, st)

    case Spec(mod, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc mod
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      _phi ==> Forall(formals, _psi ==> box(rest, post, env0, old, st))

    case If(test, left, right) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test ==> box(left :: rest, post, env0, old, st)
      val _right = !_test ==> box(right :: rest, post, env0, old, st)
      _left && _right

    case While(test, body, term, phi, psi) :: rest =>
      val mod = body.mod
      val mod_ = mod.toList

      val (formals, env1) = env0 havoc mod

      val _test = eval(test, env1, env1 :: old, st)

      val hyp = Spec(mod_, phi, !test && psi)

      val _phi1 = eval(phi, env1, old, st)
      val _psi1 = eval(psi, env1, env1 :: old, st)

      val use = Forall(formals, _psi1 ==> box(rest, post, env1, old, st))
      val base = Forall(formals, (!_test && _phi1) ==> _psi1)
      val step = Forall(formals, (_test && _phi1) ==> box(List(body, hyp), psi, env1, env1 :: old, st))

      use && base && step
  }

  def dia(progs: List[Prog], post: Expr, env0: Env, old: List[Env], st: State): Expr = progs match {
    case Nil =>
      val _psi = eval(post, env0, old, st)
      _psi

    case Block(progs) :: rest =>
      dia(progs ++ rest, post, env0, old, st)

    case Assign(lets) :: rest =>
      val pairs = lets map (eval(_, env0, old, st))
      val (xs, _es) = pairs unzip
      val env1 = env0 assign (xs, _es)
      dia(rest, post, env1, old, st)

    case Spec(mod, phi, psi) :: rest =>
      val (formals, env1) = env0 havoc mod
      val _phi = eval(phi, env0, old, st)
      val _psi = eval(psi, env1, env0 :: old, st)
      _phi && Exists(formals, _psi && dia(rest, post, env0, old, st))

    case If(test, left, right) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test && dia(left :: rest, post, env0, old, st)
      val _right = !_test && dia(right :: rest, post, env0, old, st)
      _left || _right

    case While(test, body, term, phi, psi) :: rest =>
      val mod = body.mod
      val mod_ = mod.toList

      val (formals, env1) = env0 havoc mod

      val _test = eval(test, env1, env1 :: old, st)
      val decrease = test ==> (0 <= term && term < Old(term))

      val hyp = Spec(mod_, decrease && phi, !test && psi)

      val _phi0 = eval(phi, env0, old, st)
      val _phi1 = eval(phi, env1, old, st)
      val _psi1 = eval(psi, env1, env1 :: old, st)

      val use = _phi0 && Exists(formals, _psi1 && dia(rest, post, env1, old, st))
      val base = Forall(formals, (!_test && _phi1) ==> _psi1)
      val step = Forall(formals, (_test && _phi1) ==> wp(List(body, hyp), psi, env1, env1 :: old, st))

      use && base && step
  }
}