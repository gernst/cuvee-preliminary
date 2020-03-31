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

case class Sel(id: Id, typ: Type) {
  override def toString = Printer.sel(id, typ)
}

case class Constr(id: Id, sels: List[Sel]) {
  override def toString = Printer.constr(id, sels)
}

case class Datatype(params: List[Sort], constrs: List[Constr]) {
  override def toString = Printer.datatype(params, constrs)

  /**
   * Return a pattern for x for each constructor,
   * and substitutions that instantiates x for each inductive hypotesis.
   */
  def induction(x: Id, sort: Sort) = {
    for (Constr(id, sels) <- constrs) yield {
      // fresh variables for each constructor argument, named as the selectors
      val args = for (Sel(id, typ) <- sels)
        yield Formal(Expr.fresh(id), typ)

      val hyps = for ((arg, i) <- args.zipWithIndex if arg.typ == sort)
        yield i

      (id, args, hyps)
    }
  }
}

case class Arity(sort: Sort, arity: Int) {
  override def toString = Printer.arity(sort, arity)
}

sealed trait Pat {
  def bound: Set[Id]
  def toExpr: Expr
  def rename(re: Map[Id, Id]): Pat
}

sealed trait Expr extends Expr.term {
  def ?(left: Expr, right: Expr) = Ite(this, left, right)

  def ^(that: Expr) = Exp(this, that)
  def *(that: Expr) = Times(this, that)
  def /(that: Expr) = DivBy(this, that)
  def %(that: Expr) = Mod(this, that)

  def unary_- = UMinus(this)
  def +(that: Expr) = Plus(this, that)
  def -(that: Expr) = Minus(this, that)
  def ===(that: Expr) = Eq(this, that)
  def !==(that: Expr) = !(this === that)

  def <=(that: Expr) = Le(this, that)
  def <(that: Expr) = Lt(this, that)
  def >=(that: Expr) = Ge(this, that)
  def >(that: Expr) = Gt(this, that)

  def unary_! = Not(this)
  def &&(that: Expr) = And(this, that)
  def ||(that: Expr) = Or(this, that)
  def ==>(that: Expr) = Imp(this, that)

  def isNil = this === Id.nil
  def ::(that: Expr) = Cons(this, that)

  def head = Head(this)
  def tail = Tail(this)

  def select(index: Expr) = Select(this, index)
  def store(index: Expr, arg: Expr) = Store(this, index, arg)
}

object Expr extends Parseable[Expr](Parser.expr) with Alpha[Expr, Id] {

  def fresh(name: String) = {
    Id(name, Some(nextIndex))
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

  val _eq = Id("=")
  val exp = Id("exp")
  val abs = Id("abs")
  val times = Id("*")
  val divBy = Id("/")
  val mod = Id("mod")

  val uminus = Id("-")
  val plus = Id("+")
  val minus = Id("-")

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
  val head = Id("head")
  val tail = Id("tail")
  val last = Id("last")
  val init = Id("init")
}

case class Formal(id: Id, typ: Type) {
  def prime = Formal(id.prime, typ)
  def rename(re: Map[Id, Id]) = Formal(id rename re, typ)
  def subst(su: Map[Id, Expr]) = id subst su
  override def toString = sexpr(id, typ)
}

case class Num(value: BigInt) extends Expr {
  def free = Set()
  def rename(re: Map[Id, Id]) = this
  def subst(su: Map[Id, Expr]) = this
  override def toString = value.toString
}

object Num extends (BigInt => Num) {
  val zero = Num(0)
  val one = Num(1)
}

case class Attr(name: String, arg: Option[String]) {
  def flat = arg match {
    case None => List(name)
    case Some(arg) => List(name, arg)
  }
}

case class Note(expr: Expr, attrs: List[Attr]) extends Expr {
  def free = expr.free
  def rename(re: Map[Id, Id]) = Note(expr rename re, attrs)
  def subst(su: Map[Id, Expr]) = Note(expr subst su, attrs)
  override def toString = sexpr("!", expr :: (attrs flatMap (_.flat)))
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

  def zip(lefts: List[Expr], rights: List[Expr]): List[Expr] = {
    zip(lefts zip rights)
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

object Not extends Sugar.unary(Id.not)

object Imp extends Sugar.associative(Id.imp, Assoc.right) {
  def flat(phis: List[Expr]): Expr = {
    flat(Nil, phis)
  }

  def flat(assms: List[Expr], phis: List[Expr]): Expr = phis match {
    case phi :: Nil => Imp(And(assms), phi)
    case phi :: rest => flat(phi :: assms, rest)
  }
}

object And extends Sugar.commutative(Id.and, True, Assoc.left)
object Or extends Sugar.commutative(Id.or, False, Assoc.left)

object UMinus extends Sugar.unary(Id.uminus)
object Plus extends Sugar.commutative(Id.plus, Num.zero, Assoc.left)
object Minus extends Sugar.associative(Id.minus, Assoc.left)
object Times extends Sugar.commutative(Id.times, Num.one, Assoc.left)
object DivBy extends Sugar.associative(Id.divBy, Assoc.left)
object Mod extends Sugar.associative(Id.mod, Assoc.left)
object Exp extends Sugar.associative(Id.exp, Assoc.right)

object Lt extends Sugar.binary(Id.lt)
object Le extends Sugar.binary(Id.le)
object Gt extends Sugar.binary(Id.gt)
object Ge extends Sugar.binary(Id.ge)

object Head extends Sugar.unary(Id.head)
object Tail extends Sugar.unary(Id.tail)
object Cons extends Sugar.binary(Id.cons)

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
  ensure(!args.isEmpty, "no arguments to application", this)
  def free = Set(args flatMap (_.free): _*)
  def rename(re: Map[Id, Id]) = App(fun, args map (_ rename re))
  def subst(su: Map[Id, Expr]) = App(fun, args map (_ subst su))
  override def toString = sexpr(fun, args: _*)
}

object App {
  // Needed only in Tests
  def apply(fun: Id, args: Expr*): App = {
    App(fun, args.toList)
  }
}

case class UnApp(fun: Id, args: List[Id]) extends Pat {
  def bound = Set(args: _*)
  def toExpr = App(fun, args)
  def rename(re: Map[Id, Id]) = UnApp(fun rename re, args map (_ rename re))
  override def toString = sexpr(fun, args: _*)
}

object Apps extends (List[Expr] => Expr) {
  def apply(exprs: List[Expr]): Expr = exprs match {
    case Nil => error("empty application")
    case List(expr) => expr
    case List(Id.uminus, arg) => UMinus(arg)
    case List(Id._eq, arg1, arg2) => Eq(arg1, arg2)
    case Id.plus :: args => Plus(args)
    case Id.minus :: args => Minus(args)
    case Id.times :: args => Times(args)
    case Id.divBy :: args => DivBy(args)
    case Id.and :: args => And(args)
    case Id.or :: args => Or(args)
    case Id.imp :: args => Imp(args)
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

  def apply(vars: List[Id], types: List[Type], body: Expr): Expr = {
    ensure(vars.length == types.length, "length mismatch", vars, types)
    val formals = for ((x, t) <- (vars zip types)) yield Formal(x, t)
    apply(formals, body)
  }

  def apply(formals: List[Formal], body: Expr): Expr = {
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

sealed trait Modality extends ((Prog, Expr) => Expr) {
  def quant: Quant

  def apply(prog: Prog, post: Expr) = {
    Post(this, prog, post)
  }

  def apply(body: Body, post: Expr) = {
    quant(
      body.locals,
      Post(this, body.prog, post))
  }

  def unapply(expr: Post) = expr match {
    case Post(how, prog, post) if how == this =>
      Some((prog, post))
    case _ =>
      None
  }
}

case object WP extends Modality {
  def quant = Forall
  override def toString = "wp"
}

case object Dia extends Modality {
  def quant = Exists
  override def toString = "dia"
}

case object Box extends Modality {
  def quant = Forall
  override def toString = "box"
}

case class Post(how: Modality, prog: Prog, post: Expr) extends Expr {
  def free = prog.read ++ post.free // XXX: overapproximation
  def rename(re: Map[Id, Id]) = Post(how, prog replace re, post rename re)
  def subst(su: Map[Id, Expr]) = ???
  override def toString = sexpr(how, prog, post)
}

sealed trait Prog {
  def mod: Set[Id]
  def read: Set[Id]
  def replace(re: Map[Id, Id]): Prog
}

object Prog extends Parseable(Parser.prog)

case class Block(progs: List[Prog], withOld: Boolean = false) extends Prog {
  def mod = Set(progs flatMap (_.mod): _*)
  def read = Set(progs flatMap (_.read): _*)
  def replace(re: Map[Id, Id]) = Block(progs map (_ replace re), withOld)
  def ++(that: Block) = Block(this.progs ++ that.progs, withOld)
  override def toString = sexpr("block", progs: _*)
}

/* object Block extends (List[Prog] => Block) {
  def apply(progs: List[Prog]): Block = {
    Block(progs, false)
  }
} */

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

object Assign extends (List[Pair] => Assign) {
  def from = (pairs: List[(Expr, Expr)]) => Assign(pairs map shift)

  def shift(xe: (Expr, Expr)): Pair = xe match {
    case (x: Id, e) =>
      Pair(x, e)
    case (Select(a, i), e) =>
      shift(a, Store(a, i, e))
    case (x, e) =>
      error("not an access form on left-hand side of assignment", x, e)
  }
}

case class Spec(xs: List[Id], pre: Expr, post: Expr) extends Prog {
  def mod = xs.toSet
  def read = pre.free ++ (post.free -- mod)
  def replace(re: Map[Id, Id]) = Spec(xs map (_ rename re), pre rename re, post rename re)
  override def toString = sexpr("spec", sexpr(xs), pre, post)
}

/* Can't have this as Spec currently, since we need to prove existence of such a value, and that requires the types to be known.
 * Also, within a Box, existence of such xs is not needed to be shown.
 */
case class Choose(xs: List[Id], phi: Expr) extends Prog {
  def mod = xs.toSet
  def read = phi.free -- mod
  def replace(re: Map[Id, Id]) = Choose(xs map (_ rename re), phi rename re)
  override def toString = sexpr("choose", sexpr(xs), phi)
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
