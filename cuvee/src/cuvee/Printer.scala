package cuvee

object Printer {
  def solver(solver: Solver) = solver.log.mkString("\n")

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
  
  def id(id: Id) = mangle(id)

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

  def sel(id: Id, typ: Type) = {
    sexpr(id, typ)
  }

  def constr(id: Id, sels: List[Sel]) = {
    sexpr(id, sels: _*)
  }

  def arity(sort: Sort, arity: Int) = {
    sexpr(sort, arity)
  }

  def datatype(params: List[Sort], constrs: List[Constr]) = {
    if (params.isEmpty) sexpr(constrs)
    else sexpr("par", sexpr(params), sexpr(constrs))
  }

  def declare(arities: List[Arity], decls: List[Datatype]) = {
    sexpr("declare-datatypes", sexpr(arities), sexpr(decls))
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

