package tacas2020.caclulus

import tacas2020.syntax._
import tacas2020.pure._

object Exec {
  import Eval._

  def wp(progs: List[Prog], post: Expr, st: State, env0: Store): Pure = progs match {
    case Nil =>
      val _post = eval(post, st, env0)
      _post

    case Assign(xs, es) :: rest =>
      val _es = es map (eval(_, st, env0))
      val env1 = env0 ++ (xs zip _es)
      wp(rest, post, st, env1)

    case Let(ps, es) :: rest =>
      val xs = ps map (_.ident)
      val _es = es map (eval(_, st, env0))
      val env1 = env0 ++ (xs zip _es)
      wp(rest, post, st, env1)

    case Spec(phi, mod, psi) :: rest =>
      val _pre = eval(phi, st, env0)
      val (_env, st1) = st havoc mod
      val _post = eval(psi, st1)
      _pre && pure.All(mod map _env, _post ==> wp(rest, post, st, env0))

    case If(test, left, right) :: rest =>
      val _test = eval(test, st, env0)
      val _left = _test ==> wp(left.open ++ rest, post, st, env0)
      val _right = !_test ==> wp(right.open ++ rest, post, st, env0)
      _left && _right
  }

  def box(progs: List[Prog], post: Expr, st: State, env0: Store): Pure = progs match {
    case Nil =>
      val _post = eval(post, st, env0)
      _post

    case Assign(xs, es) :: rest =>
      val _es = es map (eval(_, st, env0))
      val env1 = env0 ++ (xs zip _es)
      box(rest, post, st, env1)

    case Let(ps, es) :: rest =>
      val xs = ps map (_.ident)
      val _es = es map (eval(_, st, env0))
      val env1 = env0 ++ (xs zip _es)
      box(rest, post, st, env1)

    case Spec(phi, mod, psi) :: rest =>
      val (env0, st1) = st havoc mod
      val _pre = eval(phi, st, env0)
      val _mod = mod map env0
      val _post = eval(psi, st1)
      _pre ==> pure.All(mod map env0, _post ==> box(rest, post, st, env0))

    case If(test, left, right) :: rest =>
      val _test = eval(test, st, env0)
      val _left = _test ==> box(left.open ++ rest, post, st, env0)
      val _right = !_test ==> box(right.open ++ rest, post, st, env0)
      _left && _right
  }

  def dia(progs: List[Prog], post: Expr, st: State, env0: Store): Pure = progs match {
    case Nil =>
      val _post = eval(post, st, env0)
      _post

    case Assign(xs, es) :: rest =>
      val _es = es map (eval(_, st, env0))
      val env1 = env0 ++ (xs zip _es)
      dia(rest, post, st, env1)

    case Let(ps, es) :: rest =>
      val xs = ps map (_.ident)
      val _es = es map (eval(_, st, env0))
      val env1 = env0 ++ (xs zip _es)
      dia(rest, post, st, env1)

    case Spec(phi, mod, psi) :: rest =>
      val (env0, st1) = st havoc mod
      val _pre = eval(phi, st, env0)
      val _mod = mod map env0
      val _post = eval(psi, st1)
      _pre && pure.Ex(_mod, _post && dia(rest, post, st, env0))

    case If(test, left, right) :: rest =>
      val _test = eval(test, st, env0)
      val _left = _test && dia(left.open ++ rest, post, st, env0)
      val _right = !_test && dia(right.open ++ rest, post, st, env0)
      _left || _right
  }
}