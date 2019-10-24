package tacas2020.caclulus

import tacas2020.Error
import tacas2020.pure._
import tacas2020.syntax._

object Eval {
  import Exec._

  // Note: env captures logical variables and is preserved on old
  def eval(expr: Expr, st: State, env: Store = Map()): Pure = expr match {
    case Old(expr) =>
      st.old match {
        case None =>
          throw Error("no old state", expr, st)
        case Some(old) =>
          eval(expr, old, env)
      }

    case Lit(value) =>
      Const.int(value)

    case id: Id if (env contains id) =>
      env(id)

    case id: Id if (st.vars contains id) =>
      st store id

    case id: Id if (st.funs contains id) =>
      val fun = st funs id
      fun()

    case id: Id =>
      throw Error("undeclared identifier", id, st)

    case Call(id, args) if !(st.funs contains id) =>
      throw Error("undefined function", id, st)

    case Call(id, args) =>
      val fun = st funs id
      fun(args map (eval(_, st, env)): _*)

    case expr @ All(params, body) =>
      val ids = expr.bound
      val _env = st arbitrary params
      Bind.all(ids map _env, eval(body, st, env ++ _env))

    case expr @ Ex(params, body) =>
      val ids = expr.bound
      val _env = st arbitrary params
      Bind.ex(ids map _env, eval(body, st, env ++ _env))

    case WP(prog, post) =>
      wp(prog.open, post, st, env)

    case Box(prog, post) =>
      box(prog.open, post, st, env)

    case Dia(prog, post) =>
      dia(prog.open, post, st, env)
  }
}