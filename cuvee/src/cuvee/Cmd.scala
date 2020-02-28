package cuvee

sealed trait Cmd
sealed trait Decl extends Cmd
sealed trait Def extends Cmd

object Cmd extends Parseable(Parser.cmd)
object Script extends Parseable(Parser.script)

case class SetLogic(logic: String) extends Cmd {
  override def toString = Printer.setLogic(logic)
}

case class SetOption(args: List[String]) extends Cmd {
  override def toString = Printer.setOption(args)
}

case class SetInfo(attr: String, arg: Option[Any]) extends Cmd {
  override def toString = Printer.setInfo(attr, arg)
}

object GetModel extends Cmd {
  override def toString = Printer.model()
}

case object Exit extends Cmd {
  override def toString = Printer.exit()
}

case object Reset extends Cmd {
  override def toString = Printer.reset()
}

case class Push(depth: Int) extends Cmd {
  override def toString = Printer.push(depth)
}

case class Pop(depth: Int) extends Cmd {
  override def toString = Printer.pop(depth)
}

case object GetAssertions extends Cmd {
  override def toString = Printer.assertions()
}

case class CheckSat(expected: Option[IsSat] = None) extends Cmd {
  override def toString = Printer.check()
}

case class Assert(expr: Expr) extends Cmd {
  override def toString = Printer.assert(expr)
}

object CounterExample extends ((Expr, Prog, Expr) => Cmd) {
  def apply(pre: Expr, prog: Prog, post: Expr): Cmd = prog match {
    case While(test, body, after, term, phi, psi) =>
      val _pre = if (phi == True) pre else phi
      val _post = if (psi == True) post else psi
      val _loop = While(test, body, after, term, _pre, _post)
      val _prog = Block(List(_loop), withOld = true)

      if (term == Num(0))
        Assert(!(pre ==> Box(_prog, post)))
      else
        Assert(!(pre ==> WP(_prog, post)))

    case _ =>
      val _prog = Block(List(prog), withOld = true)
      Assert(!(pre ==> WP(_prog, post)))
  }
}

case class DeclareSort(sort: Sort, arity: Int) extends Decl {
  override def toString = Printer.declare(sort, arity)
}

case class DefineSort(sort: Sort, args: List[Sort], body: Type) extends Def {
  override def toString = Printer.define(sort, args, body)
}

case class DeclareFun(id: Id, args: List[Type], res: Type) extends Decl {
  override def toString = Printer.declare(id, args, res)
}

case class DefineFun(id: Id, formals: List[Formal], res: Type, body: Expr) extends Def {
  override def toString = Printer.define(id, formals, res, body, false)
}

case class DefineFunRec(id: Id, formals: List[Formal], res: Type, body: Expr) extends Def {
  override def toString = Printer.define(id, formals, res, body, true)
}

case class DeclareDatatypes(arities: List[Arity], decls: List[Datatype]) extends Def {
  override def toString = Printer.declare(arities, decls)
}

case class DefineClass(sort: Sort, obj: Obj) extends Def {
  override def toString = Printer.define(sort, obj)
}

case class VerifyProc(id: Id) extends Cmd {
  
}

sealed trait Sim
object Sim {
  case class byFun(fun: Id, synth: Option[Recipe]) extends Sim
  case class byExpr(as: List[Formal], cs: List[Formal], phi: Expr) extends Sim
}

case class VerifyRefinement(spec: Sort, impl: Sort, sim: Sim) extends Cmd {
  
}

/*
case class DeclareProc(id: Id, in: List[Type], ref: List[Type], out: List[Type]) extends Cmd {
  override def toString = sexpr("declare-proc", id, sexpr(in), ref)
}
*/

case class DefineProc(id: Id, proc: Proc) extends Def {
  def in = proc.in
  def out = proc.out
  def body = proc.body
}

/*
case class DefineProcRec(id: Id, in: List[Type], ref: List[Formal], body: Expr) extends Cmd {
  override def toString = sexpr("define-proc-rec", id, sexpr(in), ref, body)
}
*/

sealed trait Res

sealed trait IsSat extends Res
sealed trait Ack extends Res

object Res extends Parseable(Parser.res)
object IsSat extends Parseable(Parser.is_sat)
object Ack extends Parseable(Parser.ack)

object Assertions extends Parseable(Parser.assertions) with (List[Expr] => Assertions)
object Model extends Parseable(Parser.model) with (List[Def] => Model)

case object Success extends Ack {
  override def toString = "success"
}

case object Unsupported extends Ack {
  override def toString = "unsupported"
}

case class Error(info: Seq[Any]) extends Exception(info.mkString("\n")) with Res {
  override def toString = Printer.error(info)
}

object Error extends (String => Ack) {
  def apply(msg: String): Ack = {
    throw Error(Seq(msg))
  }
}

case object Sat extends IsSat {
  override def toString = Printer.sat()
}

case object Unknown extends IsSat {
  override def toString = Printer.unknown()
}

case object Unsat extends IsSat {
  override def toString = Printer.unsat()
}

case class Assertions(exprs: List[Expr]) extends Res {
  override def toString = Printer.assertions(exprs)
}

case class Model(defs: List[Def]) extends Res {
  override def toString = Printer.model()
}
