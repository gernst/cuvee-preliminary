package tacas2020

case class Env(su: Map[Id, Expr], ty: Map[Id, Type]) {
  def contains(id: Id) = su contains id
  def apply(id: Id) = su apply id

  def check(xs: Iterable[Id]) {
    for (x <- xs)
      ensure(su contains x, "undeclared program variable", x, xs)
  }

  def assign(xs: List[Id], es: List[Expr]): Env = {
    check(xs)
    Env(su ++ (xs zip es), ty)
  }

  def havoc(xs: List[Id]): (List[Formal], Env) = {
    check(xs)
    val re = Expr.fresh(xs)
    val formals = xs map (x => Formal(x, ty(x)))
    val env = Env(su ++ re, ty)
    (formals, env)
  }
}

object Eval {
  def eval(expr: Expr, env: Env, old: List[Env], st: State): Expr = expr match {
    case num: Num =>
      num

    case id: Id if (env contains id) =>
      env(id)

    case id: Id if (st.funs contains id) =>
      val (args, res) = st funs id
      ensure(args.isEmpty, "not constant", expr, env, st)
      id

    case Old(expr) =>
      old match {
        case Nil =>
          error("no old state", expr, env, st)
        case env :: old =>
          eval(expr, env, old, st)
      }

    case id: Id =>
      error("unknown identifier", expr, env, st)

    case App(id, args) if (st.funs contains id) =>
      val (types, res) = st funs id
      ensure(args.length == types.length, "wrong number of arguments", expr, env, st)
      App(id, args map (eval(_, env, old, st)))

    case App(id, args) =>
      throw Error("unknown function", expr, env, st)

    /* case expr @ All(params, body) =>
      val ids = expr.bound
      val _env = st arbitrary params
      Bind.all(ids map _env, eval(body, st, env ++ _env))

    case expr @ Ex(params, body) =>
      val ids = expr.bound
      val _env = st arbitrary params
      Bind.ex(ids map _env, eval(body, st, env ++ _env)) */

    case WP(Block(prog), post) =>
      wp(prog, post, env, old, st)

    /* case Box(prog, post) =>
      box(prog.open, post, st, env)

    case Dia(prog, post) =>
      dia(prog.open, post, st, env) */
  }

  def wp(progs: List[Prog], post: Expr, env0: Env, old: List[Env], st: State): Expr = progs match {
    case Nil =>
      val _post = eval(post, env0, old, st)
      _post

    case Assign(xs, es) :: rest =>
      val _es = es map (eval(_, env0, old, st))
      val env1 = env0 assign (xs, _es)
      wp(rest, post, env1, old, st)

    case Spec(mod, phi, psi) :: rest =>
      val _pre = eval(phi, env0, old, st)
      val (formals, env1) = env0 havoc mod
      val _post = eval(psi, env1, old, st)
      _pre && Forall(formals, _post ==> wp(rest, post, env1, old, st))

    case If(test, Block(left), Block(right)) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test ==> wp(left ++ rest, post, env0, old, st)
      val _right = !_test ==> wp(right ++ rest, post, env0, old, st)
      _left && _right

    case While(test, body, term, pre, post) :: rest =>
      ???
  }
  
  def box(progs: List[Prog], post: Expr, env0: Env, old: List[Env], st: State): Expr = progs match {
    case Nil =>
      val _post = eval(post, env0, old, st)
      _post

    case Assign(xs, es) :: rest =>
      val _es = es map (eval(_, env0, old, st))
      val env1 = env0 assign (xs, _es)
      box(rest,  post, env1, old, st)


    case Spec(mod, phi, psi) :: rest =>
      val _pre = eval(phi, env0, old, st)
      val (formals, env1) = env0 havoc mod
      val _post = eval(psi, env1, old, st)
      _pre ==> Forall(formals, _post ==> box(rest, post,  env0, old, st))

    case If(test, Block(left), Block(right)) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test ==> box(left ++ rest, post,  env0, old, st)
      val _right = !_test ==> box(right ++ rest, post,  env0, old, st)
      _left && _right
  }

  def dia(progs: List[Prog], post: Expr, env0: Env, old: List[Env], st: State): Expr = progs match {
    case Nil =>
      val _post = eval(post, env0, old, st)
      _post

    case Assign(xs, es) :: rest =>
      val _es = es map (eval(_, env0, old, st))
      val env1 = env0 assign (xs, _es)
      dia(rest, post, env1, old, st)


    case Spec(mod, phi, psi) :: rest =>
      val _pre = eval(phi, env0, old, st)
      val (formals, env1) = env0 havoc mod
      val _post = eval(psi, env1, old, st)
      _pre && Exists(formals, _post && dia(rest, post,  env0, old, st))

    case If(test, Block(left), Block(right)) :: rest =>
      val _test = eval(test, env0, old, st)
      val _left = _test && dia(left ++ rest, post,  env0, old, st)
      val _right = !_test && dia(right ++ rest, post,  env0, old, st)
      _left || _right
  }
}