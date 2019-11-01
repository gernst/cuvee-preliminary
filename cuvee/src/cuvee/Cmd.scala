package cuvee

sealed trait Cmd
sealed trait Decl extends Cmd
sealed trait Def extends Cmd

object Cmd {
  def from(text: String) = {
    import Parser.whitespace
    import Parser.cmd
    cmd.parseAll(text)
  }
}

case class SetLogic(logic: String) extends Cmd {
  override def toString = sexpr("set-logic", logic)
}

case class SetOption(args: List[String]) extends Cmd {
  override def toString = sexpr("set-option", args: _*)
}

object GetModel extends Cmd {
  override def toString = sexpr("get-model")
}

case object Exit extends Cmd {
  override def toString = sexpr("exit")
}

case object Reset extends Cmd {
  override def toString = sexpr("reset")
}

case object Push extends Cmd {
  override def toString = sexpr("push")
}

case object Pop extends Cmd {
  override def toString = sexpr("pop")
}

case object GetAssertions extends Cmd {
  override def toString = sexpr("get-assertions")
}

case object CheckSat extends Cmd {
  override def toString = sexpr("check-sat")
}

case class Assert(expr: Expr) extends Cmd {
  override def toString = sexpr("assert", expr)
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
      Assert(!(pre ==> WP(prog, post)))
  }
}

case class DeclareSort(sort: Sort, arity: Int) extends Decl {
  override def toString = sexpr("declare-sort", sort, arity)
}

case class DefineSort(sort: Sort, args: List[Sort], body: Type) extends Def {
  override def toString = sexpr("declare-fun", sort, sexpr(args), body)
}

case class DeclareFun(id: Id, args: List[Type], res: Type) extends Decl {
  override def toString = sexpr("declare-fun", id, sexpr(args), res)
}

case class DefineFun(id: Id, args: List[Formal], res: Type, body: Expr) extends Def {
  override def toString = sexpr("define-fun", id, sexpr(args), res, body)
}

case class DefineFunRec(id: Id, args: List[Formal], res: Type, body: Expr) extends Def {
  override def toString = sexpr("define-fun-rec", id, sexpr(args), res, body)
}

/*
case class DeclareProc(id: Id, in: List[Type], ref: List[Type], out: List[Type]) extends Cmd {
  override def toString = sexpr("declare-proc", id, sexpr(in), ref)
}

case class DefineProc(id: Id, in: List[Type], ref: List[Formal], body: Expr) extends Cmd {
  override def toString = sexpr("define-proc", id, sexpr(in), ref, body)
}

case class DefineProcRec(id: Id, in: List[Type], ref: List[Formal], body: Expr) extends Cmd {
  override def toString = sexpr("define-proc-rec", id, sexpr(in), ref, body)
}
*/

sealed trait Res

sealed trait IsSat extends Res
sealed trait Ack extends Res

object Res {
  def from(text: String) = {
    import Parser.whitespace
    import Parser.res
    res.parseAll(text)
  }
}

case object Success extends Ack {
  override def toString = "success"
}

case object Unsupported extends Ack {
  override def toString = "unsupported"
}

case class Error(info: Seq[Any]) extends Exception with Ack {
  override def toString = {
    info.mkString("(error \"", ", ", "\")")
  }
}

object Error extends (String => Error) {
  def apply(msg: String): Error = {
    Error(Seq(msg))
  }
}

case object Sat extends IsSat {
  override def toString = "sat"
}

case object Unknown extends IsSat {
  override def toString = "unknown"
}

case object Unsat extends IsSat {
  override def toString = "unsat"
}

case class Assertions(exprs: List[Expr]) extends Res {
  override def toString = sexpr(exprs)
}

case class Model(defs: List[Def]) extends Res {
  override def toString = sexpr(defs)
}
