package cuvee

object Printer {
  def setLogic(logic: String) = sexpr("set-logic", logic)
  def setOption(args: List[String]) = sexpr("set-option", args: _*)

  def reset() = sexpr("reset")
  def push() = sexpr("push")
  def pop() = sexpr("pop")
  def exit() = sexpr("exit")

  def check() = sexpr("check-sat")
  def assertions() = sexpr("get-assertions")
  def model() = sexpr("get-model")

  def assert(expr: Expr) = sexpr("assert", expr)

  def declare(sort: Sort, arity: Int) = {
    sexpr("declare-sort", sort, arity)
  }

  def define(sort: Sort, args: List[Sort], body: Type) = {
    sexpr("define-sort", sort, sexpr(args), body)
  }

  def declare(id: Id, args: List[Type], res: Type) = {
    sexpr("declare-fun", id, sexpr(args), res)
  }

  def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean) = {
    if (!rec) sexpr("define-fun", id, sexpr(formals), res, body)
    else sexpr("define-fun-rec", id, sexpr(formals), res, body)
  }

  def sat() = "sat"
  def unsat() = "unsat"
  def unknown() = "unknown"

  def assertions(exprs: List[Expr]) = {
    sexpr(exprs)
  }

  def model(defs: List[Def]) = {
    sexpr("model", sexpr(defs))
  }

  def error(info: Seq[Any]) = {
    info.mkString("(error \"", ", ", "\")")
  }
}

