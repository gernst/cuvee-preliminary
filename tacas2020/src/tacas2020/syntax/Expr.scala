package tacas2020.syntax

import tacas2020.Alpha
import tacas2020.pure.Sort
import tacas2020.StringOps

sealed trait Expr extends Expr.term {
  def ?(left: Expr, right: Expr) = Call("?:", this, left, right)

  def ^(that: Expr) = Call(Id.exp, this, that)
  def *(that: Expr) = Call(Id.times, this, that)
  def /(that: Expr) = Call(Id.divBy, this, that)
  def %(that: Expr) = Call(Id.mod, this, that)

  def unary_- = Call(Id.uminus, this)
  def +(that: Expr) = Call(Id.plus, this, that)
  def -(that: Expr) = Call(Id.minus, this, that)
  def ===(that: Expr) = Call(Id._eq, this, that)
  def !==(that: Expr) = !(this === that)

  def <=(that: Expr) = Call(Id.le, this, that)
  def <(that: Expr) = Call(Id.lt, this, that)
  def >=(that: Expr) = Call(Id.ge, this, that)
  def >(that: Expr) = Call(Id.gt, this, that)

  def unary_! = Call(Id.not, this)
  def &&(that: Expr) = Call(Id.and, this, that)
  def ||(that: Expr) = Call(Id.or, this, that)
  def ==>(that: Expr) = Call(Id.imp, this, that)

  def isNil = this === Call(Id.nil)
  def ::(that: Expr) = Call(Id.cons, that, this)

  def in(that: Expr) = Call(Id.in, this, that)
  def head = Call(Id.head, this)
  def tail = Call(Id.tail, this)
  def last = Call(Id.last, this)
  def init = Call(Id.init, this)

  def select(index: Expr) = Call(Id.select, this, index)
  def store(index: Expr, arg: Expr) = Call(Id.store, this, index, arg)
}

object Expr extends Alpha[Expr, Id] {
  def eqs(lhs: Iterable[Expr], rhs: Iterable[Expr]): Expr = {
    assert(lhs.size == rhs.size)
    val phis = for ((a, b) <- lhs zip rhs) yield Call("==", a, b)
    and(phis)
  }

  def and(exprs: Iterable[Expr]): Expr = {
    if (exprs.isEmpty) True
    else exprs reduce (_ && _)
  }
  
  def or(exprs: Iterable[Expr]): Expr = {
    if (exprs.isEmpty) False
    else exprs reduce (_ || _)
  }

  def ren(xs: Iterable[Formal], zs: Iterable[Id]): Ren = {
    val ys = xs map (x => x.ident)
    subst(ys, zs)
  }
}

case class Old(expr: Expr) extends Expr {
  def free = expr.free
  def rename(re: Ren) = Old(expr rename re)
  def subst(su: Subst) = Old(expr subst su)
}

case class Id(name: String, index: Option[Int] = None) extends Expr with Expr.x {
  def this(name: String) = this(name, None)
  def prime = Id(name + "'", index)
  def fresh(index: Int) = Id(name, Some(index))
  override def toString = name __ index
}

object Id {
  val ite = Id("ite")

  def _false = Id("false")
  def _true = Id("true")

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

  val nil = Id("Nil")
  val cons = Id("::")
  val in = Id("in")
  val head = Id("head")
  val tail = Id("tail")
  val last = Id("last")
  val init = Id("init")

  val select = Id("apply")
  val store = Id("updated")
}

case class Lit(num: Int) extends Expr {
  def free = Set()
  def rename(re: Ren) = this
  def subst(su: Subst) = this
}

case class Call(fun: Id, args: List[Expr]) extends Expr {
  def free = Set(args flatMap (_.free): _*)
  def rename(re: Ren) = Call(fun, args map (_ rename re))
  def subst(su: Subst) = Call(fun, args map (_ subst su))
  override def toString = fun + args.mkString("(", ", ", ")")
}

object Call {
  import scala.annotation.varargs

  @varargs
  def apply(name: String, args: Expr*): Call = {
    Call(Id(name), args.toList)
  }

  @varargs
  def apply(fun: Id, args: Expr*): Call = {
    Call(fun, args.toList)
  }
}

case class Formal(ident: Id, typ: Type) {
  def prime = Formal(ident.prime, typ)
  def fresh(index: Int) = Formal(ident fresh index, typ)
  def rename(re: Ren) = Formal(ident rename re, typ)
  override def toString = ident + ": " + typ
}

sealed trait Bind extends Expr {
  def params: Typing
  def body: Expr
  def copy(params: Typing = params, body: Expr = body): Bind
  def bound = params.keySet
  def free = body.free -- bound

  def rename(params: Typing, re: Ren): Typing = {
    params map { case (id, typ) => (id rename re, typ) }
  }
  def rename(a: Ren, re: Ren): Bind = copy(rename(params, a), body rename re)
  def subst(a: Ren, su: Subst): Bind = copy(rename(params, a), body subst su)
}

case class Ex(params: Typing, body: Expr) extends Bind with Expr.bind {
  def copy(params: Typing = params, body: Expr = body) = Ex(params, body)
}

object Ex {
  def apply(params: (Id, Type)*)(body: Expr): Ex = {
    Ex(params.toMap, body)
  }

  def apply(params: Iterable[Formal], body: Expr): Ex = {
    val pairs = params map { case Formal(id, typ) => (id, typ) }
    Ex(pairs.toMap, body)
  }
}

case class All(params: Typing, body: Expr) extends Bind with Expr.bind {
  def copy(params: Typing = params, body: Expr = body) = All(params, body)
}

object All {
  def apply(params: (Id, Type)*)(body: Expr): All = {
    All(params.toMap, body)
  }

  def apply(params: Iterable[Formal], body: Expr): All = {
    val pairs = params map { case Formal(id, typ) => (id, typ) }
    All(pairs.toMap, body)
  }
}

case class WP(prog: Block, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Ren) = WP(prog rename re, post rename re)
  def subst(su: Subst) = ???
  override def toString = "wp(" + prog + ", " + post + ")"
}

case class Box(prog: Block, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Ren) = Box(prog rename re, post rename re)
  def subst(su: Subst) = ???
  override def toString = "box(" + prog + ", " + post + ")"
}

case class Dia(prog: Block, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Ren) = Dia(prog rename re, post rename re)
  def subst(su: Subst) = ???
  override def toString = "dia(" + prog + ", " + post + ")"
}
