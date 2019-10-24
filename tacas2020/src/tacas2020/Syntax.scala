package tacas2020

sealed trait Type extends Type.term {

}

case class Sort(name: String, index: Option[Int] = None) extends Type with Type.x {
  def this(name: String) = this(name, None)
  def fresh(index: Int) = Sort(name, Some(index))
  override def toString = name __ index
}

object Sort {
  val int = Sort("Int")
  val bool = Sort("Bool")
}

object Type extends Alpha[Type, Sort] {
  case class list(elem: Type) extends Type {
    def free = elem.free
    def rename(re: TRen) = list(elem rename re)
    def subst(ty: Typing) = list(elem subst ty)
  }

  case class array(dom: Type, ran: Type) extends Type {
    def free = ran.free ++ dom.free
    def rename(re: TRen) = array(dom rename re, ran rename re)
    def subst(ty: Typing) = array(dom subst ty, ran subst ty)
  }
}

sealed trait Expr extends Expr.term {

}

object Expr extends Alpha[Expr, Id] {

}

case class Id(name: String, index: Option[Int] = None) extends Expr with Expr.x {
  def this(name: String) = this(name, None)
  def fresh(index: Int) = Id(name, Some(index))
  override def toString = name __ index
}

case class Formal(id: Id, typ: Type) {
  def rename(re: Ren) = Formal(id rename re, typ)
  override def toString = "(" + id + " " + typ + ")"
}

case class Num(value: BigInt) extends Expr {
  def free = Set()
  def rename(re: Ren) = this
  def subst(su: Subst) = this
  override def toString = value.toString
}

case class App(fun: Id, args: List[Expr]) extends Expr {
  ensure(!args.isEmpty, "no arguments", this)
  def free = Set(args flatMap (_.free): _*)
  def rename(re: Ren) = App(fun, args map (_ rename re))
  def subst(su: Subst) = App(fun, args map (_ subst su))
  override def toString = "(" + fun + " " + args.mkString(" ") + ")"
}

case class Old(expr: Expr) extends Expr {
  def free = expr.free
  def rename(re: Ren) = Old(expr rename re)
  def subst(su: Subst) = Old(expr subst su)
}

sealed trait Quant {
  def apply(params: List[Formal], body: Expr) = {
    if (params.isEmpty) body
    else if (body == True) body
    else Bind(this, params, body)
  }
}

case object Forall extends Quant {
  override def toString = "forall"
}

case object Exists extends Quant {
  override def toString = "exists"
}

case class Bind(quant: Quant, formals: List[Formal], body: Expr) extends Expr with Expr.bind {
  ensure(!formals.isEmpty, "empty binding", this)
  def bound = Set(formals map (_.id): _*)
  def free = body.free -- bound
  def rename(a: Ren, re: Ren): Bind = Bind(quant, formals map (_ rename a), body rename re)
  def subst(a: Ren, su: Subst): Bind = Bind(quant, formals map (_ rename a), body subst su)
  override def toString = "(" + quant + formals.mkString(" (", " ", ") ") + body + ")"
}

case class WP(prog: Block, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Ren) = WP(prog rename re, post rename re)
  def subst(su: Subst) = ???
  override def toString = "(wp " + prog + " " + post + ")"
}

case class Box(prog: Block, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Ren) = Box(prog rename re, post rename re)
  def subst(su: Subst) = ???
  override def toString = "(box " + prog + " " + post + ")"
}

case class Dia(prog: Block, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Ren) = Dia(prog rename re, post rename re)
  def subst(su: Subst) = ???
  override def toString = "(dia " + prog + " " + post + ")"
}

sealed trait Prog {
  def mod: Set[Id]
  def read: Set[Id]
  def rename(re: Ren): Prog
}

case class Block(progs: List[Prog]) {
  def mod = Set(progs flatMap (_.mod): _*)
  def read = Set(progs flatMap (_.read): _*)
  def rename(re: Ren) = Block(progs map (_ rename re))
  def ++(that: Block) = Block(this.progs ++ that.progs)
  override def toString = "(block " + progs.mkString(" ") + ")"
}

object Block {
  def apply(progs: Prog*): Block = {
    Block(progs.toList)
  }
}

case class Assign(xs: List[Id], es: List[Expr]) extends Prog {
  ensure(!xs.isEmpty, "empty assignment", this)
  ensure(xs.length == es.length, "unbalanced assignment", this)
  def mod = xs.toSet
  def read = Set(es flatMap (_.free): _*)
  def rename(re: Ren) = Assign(xs map (_ rename re), es map (_ rename re))
  override def toString = "(assign " + xs.mkString("(", " ", ")") + " " + es.mkString("(", " ", ")") + ")"
}

object Assign {
  def apply(x: Id, e: Expr): Assign = {
    Assign(List(x), List(e))
  }

  def apply(xes: List[(Id, Expr)]): Assign = {
    val (xs, es) = xes.unzip
    Assign(xs, es)
  }
}

case class Spec(mod: Set[Id], pre: Expr, post: Expr) extends Prog {
  def read = pre.free ++ (post.free -- mod)
  def rename(re: Ren) = Spec(mod map (_ rename re), pre rename re, post rename re)
  override def toString = "(spec " + mod.mkString(" (", " ", ") ") + pre + post + ")"
}

case class If(test: Expr, left: Block, right: Block) extends Prog {
  def mod = left.mod ++ right.mod
  def read = test.free ++ left.read ++ right.read
  def rename(re: Ren) = If(test rename re, left rename re, right rename re)
  override def toString = "(if " + test + " " + left + " " + right + ")"
}

case class While(test: Expr, body: Block, term: Expr, pre: Expr, post: Expr) {
  def mod = body.mod
  def read = test.free ++ body.read
  def rename(re: Ren) = While(test rename re, body rename re, term rename re, pre rename re, post rename re)
  override def toString = "(while " + test + " " + body + " :termination " + term + " :invariant " + pre + " :post " + post + ")"
}

sealed trait Cmd {

}

case object Exit extends Cmd {
  override def toString = "(exit)"
}

case object Reset extends Cmd {
  override def toString = "(reset)"
}

case object Push extends Cmd {
  override def toString = "(push)"
}

case object Pop extends Cmd {
  override def toString = "(pop)"
}

case object CheckSat extends Cmd {
  override def toString = "(check-sat)"
}

case class Assert(expr: Expr) extends Cmd {
  override def toString = "(assert " + expr + ")"
}

case class DeclareSort(sort: Sort, arity: Int) extends Cmd {
  override def toString = "(declare-sort " + sort + " " + arity + ")"
}

case class DefineSort(sort: Sort, args: List[Sort], body: Type) extends Cmd {
  override def toString = "(declare-fun " + sort + " " + args.mkString(" (", " ", ") ") + body + ")"
}

case class DeclareFun(id: Id, args: List[Type], res: Type) extends Cmd {
  override def toString = "(declare-fun " + id + " " + args.mkString(" (", " ", ") ") + res + ")"
}

case class DefineFun(id: Id, args: List[Formal], res: Type, body: Expr) extends Cmd {
  override def toString = "(declare-fun " + id + " " + args.mkString(" (", " ", ") ") + res + " " + body + ")"
}



