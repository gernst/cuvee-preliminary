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

object GetModel extends Cmd {
  override def toString = Printer.model()
}

case object Exit extends Cmd {
  override def toString = Printer.exit()
}

case object Reset extends Cmd {
  override def toString = Printer.reset()
}

case object Push extends Cmd {
  override def toString = Printer.push()
}

case object Pop extends Cmd {
  override def toString = Printer.pop()
}

case object GetAssertions extends Cmd {
  override def toString = Printer.assertions()
}

case object CheckSat extends Cmd {
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

case class Sel(id: Id, typ: Type) {
  override def toString = Printer.sel(id, typ)
}

case class Constr(id: Id, sels: List[Sel]) {
  override def toString = Printer.constr(id, sels)
}

case class Datatype(params: List[Sort], constrs: List[Constr]) {
  override def toString = Printer.datatype(params, constrs)
}

case class Arity(sort: Sort, arity: Int) {
  override def toString = Printer.arity(sort, arity)
}

case class DeclareDatatypes(arities: List[Arity], decls: List[Datatype]) extends Def {
  override def toString = Printer.declare(arities, decls)
}

// (declare-datatypes () ((Lst (cons (head Elem) (tail Lst)) (nil))))

/*
case class DeclareProc(id: Id, in: List[Type], ref: List[Type], out: List[Type]) extends Cmd {
  override def toString = sexpr("declare-proc", id, sexpr(in), ref)
}
*/

/**
 * Defines a procedure.
 *
 * @param id  name of the procedure
 * @param in  the list of input arguments. The ids must be unique.
 * @param out the list of output arguments. Ids may turn up multiple times, but their types must be equal.
 * @param body must modify all of (out \ in). Must at most modify (in ∪ out).
 * @param pre precondition of the procedure. May only refer to in and global identifiers.
 * @param post
 */
case class DefineProc(id: Id, in: List[Formal], out: List[Formal], body: Prog, pre: Expr, post: Expr) extends Def {

  def check = {
    val inVars = in.map(_.id)
    val duplicateInputDeclarations = inVars.groupBy(identity).filter(_._2.size > 1)
    if (duplicateInputDeclarations.nonEmpty) {
      throw Error(s"The method $id declares duplicate input parameters ${duplicateInputDeclarations.keys.mkString(", ")}")
    }

    // the outputs may have the same variable name in multiple places if the type is equal.
    // outputs may overlap with inputs but, again, the type must be equal
    val nonUniqueAgruments = (in ++ out).groupBy(_.id).filter(_._2.map(_.typ).distinct.size > 1)
    if (nonUniqueAgruments.nonEmpty) {
      throw Error(s"The method $id declares non-unique type for argument ${nonUniqueAgruments.keys.mkString(", ")}")
    }

    // procedure must at most modify its output variables
    val modifiableVariables = (in ++ out).map(_.id).distinct
    val modifiedVariables = body.mod
    val illegallyModifiedVariables = modifiedVariables.filter(!modifiableVariables.contains(_))
    if (illegallyModifiedVariables.nonEmpty) {
      throw Error(s"The method $id modifies undeclared output parameters ${illegallyModifiedVariables.mkString(", ")}")
    }

    // procedure must at least modify output variables that are not input variables
    val outputsThatMustBeSet = out.map(_.id).filter(!inVars.contains(_))
    val unsetOutputs = outputsThatMustBeSet.filter(!modifiedVariables.contains(_))
    if (unsetOutputs.nonEmpty) {
      throw Error(s"The method $id does not modify its output parameters ${unsetOutputs.mkString(", ")}")
    }
  }

  override def toString = Printer.define(id, in, out, body, pre, post)

}

object DefineProc extends ((Id, List[Formal], List[Formal], Prog, Option[Expr], Option[Expr]) => DefineProc) {
  def apply(id: Id, in: List[Formal], out: List[Formal], body: Prog, pre: Option[Expr], post: Option[Expr]): DefineProc = {
    val _pre = pre.getOrElse(True)
    val _post = post.getOrElse(True)
    DefineProc(id, in, out, body, _pre, _post)
  }
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

case class Error(info: Seq[Any]) extends Exception(info.mkString("\n")) with Ack {
  override def toString = Printer.error(info)
}

object Error extends (String => Error) {
  def apply(msg: String): Error = {
    Error(Seq(msg))
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
