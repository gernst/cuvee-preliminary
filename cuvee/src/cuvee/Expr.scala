package cuvee

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
    override def toString = sexpr("List", elem)
  }

  case class array(dom: Type, ran: Type) extends Type {
    def free = ran.free ++ dom.free
    def rename(re: Map[Sort, Sort]) = array(dom rename re, ran rename re)
    def subst(ty: Map[Sort, Type]) = array(dom subst ty, ran subst ty)
    override def toString = sexpr("Array", dom, ran)
  }
}

sealed trait Pat {
  def bound: Set[Id]
  def toExpr: Expr
  def rename(re: Map[Id, Id]): Pat
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
  def ===(that: Expr) = Eq(this, that)
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

  def select(index: Expr) = Select(this, index)
  def store(index: Expr, arg: Expr) = Store(this, index, arg)
}

object Expr extends Parseable[Expr](Parser.expr) with Alpha[Expr, Id] {
  def nnf(phi: Expr): Expr = phi match {
    case Not(True) =>
      False
    case Not(False) =>
      True
    case Not(Not(phi)) =>
      nnf(phi)
    case Not(Imp(phi, psi)) =>
      nnf(phi) && !nnf(phi)
    case Not(And.nary(args)) =>
      Or(nnf(Not(args)))
    case Not(Or.nary(args)) =>
      And(nnf(Not(args)))
    case Not(Bind(quant, formals, body)) =>
      Bind(!quant, formals, nnf(!phi))

    case Imp(phi, psi) =>
      !nnf(phi) || nnf(psi)
    case And.nary(args) =>
      val _args = And.flatten(nnf(args))
      And(_args)
    case Or.nary(args) =>
      val _args = Or.flatten(nnf(args))
      Or(_args)
    case Bind(quant, formals, body) =>
      Bind(quant, formals, nnf(body))

    case _ =>
      phi
  }

  def nnf(phis: List[Expr]): List[Expr] = {
    phis map nnf
  }
}

case class Id(name: String, index: Option[Int]) extends Expr with Pat with Expr.x {
  def bound = Set()
  def toExpr = this
  def prime = Id(name + "'", index)
  def fresh(index: Int) = Id(name, Some(index))
  override def toString = Printer.id(this)

  def :=(other: Expr) = Assign(List(Pair(this, other)))
}

object Id extends (String => Id) {
  def apply(name: String): Id = {
    Id(name, None)
  }

  val ite = Id("ite")

  val exp = Id("exp")
  val abs = Id("abs")
  val times = Id("*")
  val divBy = Id("/")
  val mod = Id("mod")

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
}

case class Formal(id: Id, typ: Type) {
  def prime = Formal(id.prime, typ)
  def rename(re: Map[Id, Id]) = Formal(id rename re, typ)
  override def toString = sexpr(id, typ)
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
  override def toString = sexpr("=", left, right)
}

object Eq extends ((Expr, Expr) => Expr) {
  def zip(pairs: List[(Expr, Expr)]): List[Expr] = {
    pairs map { case (left, right) => Eq(left, right) }
  }

  def apply(lefts: List[Expr], rights: List[Expr]): Expr = {
    ensure(lefts.size == rights.size, "equations length mismatch", lefts, rights)
    apply(lefts zip rights)
  }

  def apply(pairs: List[(Expr, Expr)]): Expr = {
    And(zip(pairs))
  }
}

case class Distinct(exprs: List[Expr]) extends Expr {
  def free = Set(exprs flatMap (_.free): _*)
  def rename(re: Map[Id, Id]) = Distinct(exprs map (_ rename re))
  def subst(su: Map[Id, Expr]) = Distinct(exprs map (_ subst su))
  override def toString = sexpr("distinct", exprs: _*)
}

object Not extends Sugar.unary(Id.not) {
  def apply(args: List[Expr]) = {
    args map this
  }
}

object Imp extends Sugar.binary(Id.imp)

object And extends Sugar.binary(Id.and) {
  def apply(args: List[Expr]) = args match {
    case List() => True
    case List(arg) => arg
    case _ => App(fun, args)
  }
}

object Or extends Sugar.binary(Id.or) {
  def apply(args: List[Expr]) = args match {
    case List() => False
    case List(arg) => arg
    case _ => App(fun, args)
  }
}

case class Ite(test: Expr, left: Expr, right: Expr) extends Expr {
  def free = test.free ++ left.free ++ right.free
  def rename(re: Map[Id, Id]) = Ite(test rename re, left rename re, right rename re)
  def subst(su: Map[Id, Expr]) = Ite(test subst su, left subst su, right subst su)
  override def toString = sexpr("ite", test, left, right)
}

case class Pair(x: Id, e: Expr) {
  def mod = Set(x)
  def free = e.free
  def rename(a: Map[Id, Id], re: Map[Id, Id]) = Pair(x rename a, e rename re)
  def subst(a: Map[Id, Id], su: Map[Id, Expr]) = Pair(x rename a, e subst su)
  def replace(re: Map[Id, Id]) = Pair(x rename re, e rename re)
  override def toString = sexpr(x, e)
}

case class Let(pairs: List[Pair], body: Expr) extends Expr with Expr.bind[Let] {
  def bound = Set(pairs map (_.x): _*)
  def free = Set(pairs flatMap (_.free): _*) ++ (body.free -- bound)
  def rename(a: Map[Id, Id], re: Map[Id, Id]) = Let(pairs map (_ rename (a, re)), body rename re)
  def subst(a: Map[Id, Id], su: Map[Id, Expr]) = Let(pairs map (_ subst (a, su)), body subst su)
  override def toString = sexpr("let", sexpr(pairs), body)
}

case class Case(pat: Pat, expr: Expr) extends Expr.bind[Case] {
  def bound = pat.bound
  def free = expr.free -- pat.bound
  def rename(a: Map[Id, Id], re: Map[Id, Id]) = Case(pat rename a, expr rename re)
  def subst(a: Map[Id, Id], su: Map[Id, Expr]) = Case(pat rename a, expr subst su)
  override def toString = sexpr(pat, expr)
}

case class Match(expr: Expr, cases: List[Case]) extends Expr {
  def free = expr.free ++ (cases flatMap (_.free))
  def rename(re: Map[Id, Id]) = Match(expr rename re, cases map (_ rename re))
  def subst(su: Map[Id, Expr]) = Match(expr subst su, cases map (_ subst su))
  override def toString = sexpr("match", expr, sexpr(cases))
}

case class Select(array: Expr, index: Expr) extends Expr {
  def free = array.free ++ index.free
  def rename(re: Map[Id, Id]) = Select(array rename re, index rename re)
  def subst(su: Map[Id, Expr]) = Select(array subst su, index subst su)
  override def toString = sexpr("select", array, index)
}

case class Store(array: Expr, index: Expr, value: Expr) extends Expr {
  def free = array.free ++ index.free ++ value.free
  def rename(re: Map[Id, Id]) = Store(array rename re, index rename re, value rename re)
  def subst(su: Map[Id, Expr]) = Store(array subst su, index subst su, value subst su)
  override def toString = sexpr("store", array, index, value)
}

case class App(fun: Id, args: List[Expr]) extends Expr {
  ensure(!args.isEmpty, "no arguments", this)
  def free = Set(args flatMap (_.free): _*)
  def rename(re: Map[Id, Id]) = App(fun, args map (_ rename re))
  def subst(su: Map[Id, Expr]) = App(fun, args map (_ subst su))
  override def toString = sexpr(fun, args: _*)
}

case class UnApp(fun: Id, args: List[Id]) extends Pat {
  def bound = Set(args: _*)
  def toExpr = App(fun, args)
  def rename(re: Map[Id, Id]) = UnApp(fun rename re, args map (_ rename re))
  override def toString = sexpr(fun, args: _*)
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

object UnApps extends (List[Id] => Pat) {
  def apply(pats: List[Id]): Pat = pats match {
    case Nil => error("empty application")
    case List(id) => id
    case fun :: args => UnApp(fun, args)
    case _ => error("higher-order pattern", pats)
  }
}

case class As(id: Id, sort: Sort) extends Expr {
  def free = id.free
  def rename(re: Map[Id, Id]) = As(id rename re, sort)
  def subst(su: Map[Id, Expr]) = id subst su
  override def toString = sexpr("as", id, sort)
}

case class Old(expr: Expr) extends Expr {
  def free = expr.free
  def rename(re: Map[Id, Id]) = Old(expr rename re)
  def subst(su: Map[Id, Expr]) = Old(expr subst su)
  override def toString = sexpr("old", expr)
}

sealed trait Quant {
  def unary_!(): Quant

  def apply(formals: List[Formal], body: Expr) = {
    val free = body.free
    val _formals = formals filter (free contains _.id)

    if (_formals.isEmpty) body
    else if (body == True) body
    else {
      Bind(this, _formals, body)
    }
  }

  def unapply(bind: Bind) = bind match {
    case Bind(quant, formals, body) if quant == this =>
      Some((formals, body))
    case _ =>
      None
  }
}

case object Forall extends Quant {
  def unary_! = Exists
  override def toString = "forall"
}

case object Exists extends Quant {
  def unary_! = Forall
  override def toString = "exists"
}

case class Bind(quant: Quant, formals: List[Formal], body: Expr) extends Expr with Expr.bind[Bind] {
  ensure(!formals.isEmpty, "empty binding", this)
  def bound = Set(formals map (_.id): _*)
  def free = body.free -- bound
  def rename(a: Map[Id, Id], re: Map[Id, Id]): Bind = Bind(quant, formals map (_ rename a), body rename re)
  def subst(a: Map[Id, Id], su: Map[Id, Expr]): Bind = Bind(quant, formals map (_ rename a), body subst su)
  override def toString = sexpr(quant, sexpr(formals), body)
}

case class WP(prog: Prog, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Map[Id, Id]) = WP(prog replace re, post rename re)
  def subst(su: Map[Id, Expr]) = ???
  override def toString = sexpr("wp", prog, post)
}

case class Box(prog: Prog, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Map[Id, Id]) = Box(prog replace re, post rename re)
  def subst(su: Map[Id, Expr]) = ???
  override def toString = sexpr("box", prog, post)
}

case class Dia(prog: Prog, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Map[Id, Id]) = Dia(prog replace re, post rename re)
  def subst(su: Map[Id, Expr]) = ???
  override def toString = sexpr("dia", prog, post)
}

sealed trait Prog {
  def mod: Set[Id]
  def read: Set[Id]
  def replace(re: Map[Id, Id]): Prog
}


object Prog extends Parseable(Parser.prog)

case class Block(progs: List[Prog], withOld: Boolean) extends Prog {
  def mod = Set(progs flatMap (_.mod): _*)
  def read = Set(progs flatMap (_.read): _*)
  def replace(re: Map[Id, Id]) = Block(progs map (_ replace re))
  def ++(that: Block) = Block(this.progs ++ that.progs)
  override def toString = sexpr("block", progs: _*)
}

object Block extends (List[Prog] => Block) {
  def apply(progs: List[Prog]): Block = {
    Block(progs, false)
  }
}

case object Break extends Prog {
  def mod = Set()
  def read = Set()
  def replace(re: Map[Id, Id]) = this
  override def toString = sexpr("break")
}

case class Assign(pairs: List[Pair]) extends Prog {
  ensure(!pairs.isEmpty, "empty assignment", this)
  def mod = Set(pairs flatMap (_.mod): _*)
  def read = Set(pairs flatMap (_.free): _*)
  def replace(re: Map[Id, Id]) = Assign(pairs map (_ replace re))
  override def toString = sexpr("assign", sexpr(pairs))
}

case class Spec(xs: List[Id], pre: Expr, post: Expr) extends Prog {
  def mod = xs.toSet
  def read = pre.free ++ (post.free -- mod)
  def replace(re: Map[Id, Id]) = Spec(xs map (_ rename re), pre rename re, post rename re)
  override def toString = sexpr("spec", sexpr(xs), pre, post)
}

object Spec extends ((List[Id], Expr, Expr) => Spec) {
  def assert = (pre: Expr) => Spec(Nil, pre, True)
  def assume = (post: Expr) => Spec(Nil, True, post)
}

case class If(test: Expr, left: Prog, right: Prog) extends Prog {
  def mod = left.mod ++ right.mod
  def read = test.free ++ left.read ++ right.read
  def replace(re: Map[Id, Id]) = If(test rename re, left replace re, right replace re)
  override def toString = sexpr("if", test, left, right)
}

object If extends ((Expr, Prog, Option[Prog]) => If) {
  def apply(test: Expr, left: Prog, right: Option[Prog]): If = right match {
    case None => If(test, left, Skip)
    case Some(right) => If(test, left, right)
  }
}

case class While(test: Expr, body: Prog, after: Prog, term: Expr, pre: Expr, post: Expr) extends Prog {
  def mod = body.mod
  def read = test.free ++ body.read
  def replace(re: Map[Id, Id]) = While(test rename re, body replace re, after replace re, term rename re, pre rename re, post rename re)
  override def toString = sexpr("while", test, body, after, ":termination", term, ":precondition", pre, ":postcondition", post)
}

object While extends ((Expr, Prog, Option[Prog], Option[Expr], Option[Expr], Option[Expr]) => While) {
  def apply(test: Expr, body: Prog, after: Option[Prog], term: Option[Expr], pre: Option[Expr], post: Option[Expr]): While = {
    val _after = after getOrElse Skip
    val _term = term getOrElse Num(0)
    val _pre = pre getOrElse True
    val _post = post getOrElse True
    While(test, body, _after, _term, _pre, _post)
  }
}

case class Call(name: Id, in: List[Expr], out: List[Id]) extends Prog {
  {
    val duplicateOuts = out.groupBy(identity).filter(_._2.size > 1)
    if (duplicateOuts.nonEmpty) {
      throw Error(s"The procedure call to $name declares duplicate output parameters ${duplicateOuts.keys.mkString(", ")}")
    }
  }

  def mod = out.toSet
  def read = in.flatMap(_.free).distinct.toSet
  def replace(re: Map[Id, Id]) = Call(name, in map (_ rename re), out map (_ rename re))
  override def toString = sexpr("call", sexpr(in), sexpr(out))
}
