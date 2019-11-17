import scala.io.StdIn

package object cuvee {
  import scala.language.implicitConversions

  def error(info: Any*) = {
    throw Error(info)
  }

  def ensure(test: Boolean, info: Any*) = {
    if (!test)
      throw Error(info)
  }

  val True = Id("true")
  val False = Id("false")
  val Skip = Block(Nil)

  implicit def toNum(value: Int) = Num(value)
  implicit def toIds(formals: List[Formal]) = formals map (_.id)

  implicit class StringOps(self: String) {
    def __(index: Option[Int]): String = index match {
      case None => self
      case Some(index) => self.toString + index
    }
  }

  implicit class SetOps[A](self: Set[A]) {
    def disjoint(that: Set[A]) = {
      (self & that).isEmpty
    }
  }

  def needsEscape(c: Char) = {
    if ('0' <= c && c <= '9') false
    else if ('a' <= c && c <= 'z') false
    else if ('A' <= c && c <= 'Z') false
    else true
  }

  def mangle(id: Id) = {
    val Id(name, index) = id
    if (name exists needsEscape) Id("|" + name + "|", index)
    else id
  }

  def sexpr(arg0: Any, args: Any*): String = {
    if (args.isEmpty) "(" + arg0 + ")"
    else "(" + arg0 + " " + args.mkString(" ") + ")"
  }

  def sexpr(args: Iterable[Any]): String = {
    args.mkString("(", " ", ")")
  }
}