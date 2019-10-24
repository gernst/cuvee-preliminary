package tacas2020

sealed trait Type extends Type.term {

}

case class Sort(name: String, index: Option[Int]) extends Type with Type.x {
  def this(name: String) = this(name, None)
  def fresh(index: Int) = Sort(name, Some(index))
  override def toString = name __ index
}

object Sort extends (String => Sort) {
  val int = Sort("Int")
  val bool = Sort("Bool")

  def apply(name: String): Sort = {
    Sort(name, None)
  }
}

object Type extends Alpha[Type, Sort] {
  case class list(elem: Type) extends Type {
    def free = elem.free
    def rename(re: Map[Sort, Sort]) = list(elem rename re)
    def subst(ty: Map[Sort, Type]) = list(elem subst ty)
  }

  case class array(dom: Type, ran: Type) extends Type {
    def free = ran.free ++ dom.free
    def rename(re: Map[Sort, Sort]) = array(dom rename re, ran rename re)
    def subst(ty: Map[Sort, Type]) = array(dom subst ty, ran subst ty)
  }
}

sealed trait Expr extends Expr.term {
  def ?(left: Expr, right: Expr) = App(Id.ite, this, left, right)

  def ^(that: Expr) = App(Id.exp, this, that)
  def *(that: Expr) = App(Id.times, this, that)
  def /(that: Expr) = App(Id.divBy, this, that)
  def %(that: Expr) = App(Id.mod, this, that)

  def unary_- = App(Id.uminus, this)
  def +(that: Expr) = App(Id.plus, this, that)
  def -(that: Expr) = App(Id.minus, this, that)
  def ===(that: Expr) = App(Id._eq, this, that)
  def !==(that: Expr) = !(this === that)

  def <=(that: Expr) = App(Id.le, this, that)
  def <(that: Expr) = App(Id.lt, this, that)
  def >=(that: Expr) = App(Id.ge, this, that)
  def >(that: Expr) = App(Id.gt, this, that)

  def unary_! = App(Id.not, this)
  def &&(that: Expr) = App(Id.and, this, that)
  def ||(that: Expr) = App(Id.or, this, that)
  def ==>(that: Expr) = App(Id.imp, this, that)

  def isNil = this === App(Id.nil)
  def ::(that: Expr) = App(Id.cons, that, this)

  def in(that: Expr) = App(Id.in, this, that)
  def head = App(Id.head, this)
  def tail = App(Id.tail, this)
  def last = App(Id.last, this)
  def init = App(Id.init, this)

  def select(index: Expr) = App(Id.select, this, index)
  def store(index: Expr, arg: Expr) = App(Id.store, this, index, arg)
}

object Expr extends Alpha[Expr, Id] {

}

case class Id(name: String, index: Option[Int]) extends Expr with Expr.x {
  def this(name: String) = this(name, None)
  def fresh(index: Int) = Id(name, Some(index))
  override def toString = name __ index
}

object Id extends (String => Id) {
  def apply(name: String): Id = {
    Id(name, None)
  }

  val ite = Id("ite")

  val exp = Id("^")
  val times = Id("*")
  val divBy = Id("/")
  val mod = Id("%")

  val uminus = Id("-")
  val plus = Id("+")
  val minus = Id("-")

  val _eq = Id("=")
  val le = Id("<=")
  val lt = Id("<")
  val ge = Id(">=")
  val gt = Id(">")

  val not = Id("not")
  val and = Id("and")
  val or = Id("or")
  val imp = Id("=>")

  val nil = Id("nil")
  val cons = Id("cons")
  val in = Id("in")
  val head = Id("head")
  val tail = Id("tail")
  val last = Id("last")
  val init = Id("init")

  val select = Id("apply")
  val store = Id("updated")
}

case class Formal(id: Id, typ: Type) {
  def rename(re: Map[Id, Id]) = Formal(id rename re, typ)
  override def toString = "(" + id + " " + typ + ")"
}

case class Num(value: BigInt) extends Expr {
  def free = Set()
  def rename(re: Map[Id, Id]) = this
  def subst(su: Map[Id, Expr]) = this
  override def toString = value.toString
}

case class Eq(left: Expr, right: Expr) extends Expr {
  def free = left.free ++ right.free
  def rename(re: Map[Id, Id]) = Eq(left rename re, right rename re)
  def subst(su: Map[Id, Expr]) = Eq(left subst su, right subst su)
  override def toString = "(= " + left + " " + right + ")"
}

case class Ite(test: Expr, left: Expr, right: Expr) extends Expr {
  def free = left.free ++ right.free
  def rename(re: Map[Id, Id]) = Ite(test rename re, left rename re, right rename re)
  def subst(su: Map[Id, Expr]) = Ite(test subst su, left subst su, right subst su)
  override def toString = "(ite " + test + " " + left + " " + right + ")"
}

case class App(fun: Id, args: List[Expr]) extends Expr {
  ensure(!args.isEmpty, "no arguments", this)
  def free = Set(args flatMap (_.free): _*)
  def rename(re: Map[Id, Id]) = App(fun, args map (_ rename re))
  def subst(su: Map[Id, Expr]) = App(fun, args map (_ subst su))
  override def toString = "(" + fun + " " + args.mkString(" ") + ")"
}

object App {
  def apply(fun: Id, args: Expr*): App = {
    App(fun, args.toList)
  }
}

object Apps extends (List[Expr] => Expr) {
  def apply(exprs: List[Expr]): Expr = exprs match {
    case Nil => error("empty application")
    case List(expr) => expr
    case (fun: Id) :: args => App(fun, args)
    case _ => error("higher-order application", exprs)
  }
}

case class Old(expr: Expr) extends Expr {
  def free = expr.free
  def rename(re: Map[Id, Id]) = Old(expr rename re)
  def subst(su: Map[Id, Expr]) = Old(expr subst su)
  override def toString = "(old " + expr + ")"
}

sealed trait Quant {
  def apply(formals: List[Formal], body: Expr) = {
    if (formals.isEmpty) body
    else if (body == True) body
    else Bind(this, formals, body)
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
  def rename(a: Map[Id, Id], re: Map[Id, Id]): Bind = Bind(quant, formals map (_ rename a), body rename re)
  def subst(a: Map[Id, Id], su: Map[Id, Expr]): Bind = Bind(quant, formals map (_ rename a), body subst su)
  override def toString = "(" + quant + formals.mkString(" (", " ", ") ") + body + ")"
}

case class WP(prog: Block, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Map[Id, Id]) = WP(prog rename re, post rename re)
  def subst(su: Map[Id, Expr]) = ???
  override def toString = "(wp " + prog + " " + post + ")"
}

case class Box(prog: Block, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Map[Id, Id]) = Box(prog rename re, post rename re)
  def subst(su: Map[Id, Expr]) = ???
  override def toString = "(box " + prog + " " + post + ")"
}

case class Dia(prog: Block, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Map[Id, Id]) = Dia(prog rename re, post rename re)
  def subst(su: Map[Id, Expr]) = ???
  override def toString = "(dia " + prog + " " + post + ")"
}

sealed trait Prog {
  def mod: Set[Id]
  def read: Set[Id]
  def rename(re: Map[Id, Id]): Prog
}

case class Block(progs: List[Prog]) {
  def mod = Set(progs flatMap (_.mod): _*)
  def read = Set(progs flatMap (_.read): _*)
  def rename(re: Map[Id, Id]) = Block(progs map (_ rename re))
  def ++(that: Block) = Block(this.progs ++ that.progs)
  override def toString = "(block " + progs.mkString(" ") + ")"
}

object Block extends (List[Prog] => Block) {
  def apply(progs: Prog*): Block = {
    Block(progs.toList)
  }
}

case class Let(x: Id, e: Expr) {
  def mod = Set(x)
  def free = e.free
  def rename(re: Map[Id, Id]) = Let(x rename re, e rename re)
  override def toString = "(" + x + " " + e + ")"
}

case class Assign(lets: List[Let]) extends Prog {
  ensure(!lets.isEmpty, "empty assignment", this)
  def mod = Set(lets flatMap (_.mod): _*)
  def read = Set(lets flatMap (_.free): _*)
  def rename(re: Map[Id, Id]) = Assign(lets map (_ rename re))
  override def toString = "(assign " + lets.mkString(" ") + ")"
}

case class Spec(xs: List[Id], pre: Expr, post: Expr) extends Prog {
  def mod = xs.toSet
  def read = pre.free ++ (post.free -- mod)
  def rename(re: Map[Id, Id]) = Spec(xs map (_ rename re), pre rename re, post rename re)
  override def toString = "(spec " + xs.mkString(" (", " ", ") ") + pre + post + ")"
}

case class If(test: Expr, left: Block, right: Block) extends Prog {
  def mod = left.mod ++ right.mod
  def read = test.free ++ left.read ++ right.read
  def rename(re: Map[Id, Id]) = If(test rename re, left rename re, right rename re)
  override def toString = "(if " + test + " " + left + " " + right + ")"
}

case class While(test: Expr, body: Block, term: Expr, pre: Expr, post: Expr) extends Prog {
  def mod = body.mod
  def read = test.free ++ body.read
  def rename(re: Map[Id, Id]) = While(test rename re, body rename re, term rename re, pre rename re, post rename re)
  override def toString = "(while " + test + " " + body + " :termination " + term + " :precondition " + pre + " :post " + post + ")"
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

case object GetAssertions extends Cmd {
  override def toString = "(get-assertions)"
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



