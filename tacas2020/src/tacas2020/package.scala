import scala.io.StdIn

package object tacas2020 {
  def in() = {
    StdIn.readLine()
  }

  def out(any: Any) {
    Console.println(any)
    Console.flush()
  }

  case class Error(info: Any*) extends Exception {
    override def toString = {
      val strings = info map ("\"" + _ + "\"")
      strings.mkString("(error ", " ", ")")
    }
  }

  def error(info: Any*) = {
    throw Error(info)
  }

  def ensure(test: Boolean, info: Any*) = {
    if (!test) throw Error(info)
  }

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