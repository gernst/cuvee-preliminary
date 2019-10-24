package object tacas2020 {
  case class Error(info: Any*) extends Exception

  def ensure(test: Boolean, info: Any*) = {
    if (!test) throw Error(info)
  }

  type TRen = Map[Sort, Sort]
  type Typing = Map[Sort, Type]

  type Ren = Map[Id, Id]
  type Subst = Map[Id, Expr]

  val True = Id("true")
  val False = Id("false")
  val Skip = Block()

  implicit class StringOps(self: String) {
    def __(index: Option[Int]): String = index match {
      case None => self
      case Some(index) => this.toString + index
    }
  }

  implicit class SetOps[A](self: Set[A]) {
    def disjoint(that: Set[A]) = {
      (self & that).isEmpty
    }
  }

  def sexpr(arg0: String, args: String*) = {
    "(" + arg0 + " " + args.mkString(" ") + ")"
  }

  def sexpr(args: Iterable[String]) = {
    args mkString ("(", " ", ")")
  }
}