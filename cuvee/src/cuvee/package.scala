import scala.io.StdIn
import java.lang.UNIXProcess

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
  implicit def toExprs(pats: List[Pat]) = pats map (_.toExpr)
  implicit def toIds(formals: List[Formal]) = formals map (_.id)
  implicit def toTypes(formals: List[Formal]) = formals map (_.typ)
  implicit def toTyping(formals: List[Formal]) = formals map (f => (f.id, f.typ))

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

  implicit class IterableOps[A](self: Iterable[A]) {
    def _insert(a: A, as: List[(A, List[A])], eq: (A, A) => Boolean): List[(A, List[A])] = as match {
      case Nil => List((a, List(a)))
      case (b, bs) :: cs if eq(a, b) => (a, b :: bs) :: cs
      case c :: cs => c :: _insert(a, cs, eq)
    }

    def _classes(eq: (A, A) => Boolean): List[(A, List[A])] = {
      self.foldLeft(Nil: List[(A, List[A])]) {
        case (as, a) => _insert(a, as, eq)
      }
    }

    def classes(eq: (A, A) => Boolean) = {
      for ((_, as) <- _classes(eq) if as.length > 1)
        yield as
    }

    def duplicates(eq: (A, A) => Boolean) = {
      classes(eq).flatten
    }
  }

  def ok = "=-+<>"

  def needsEscape(c: Char) = {
    if ('0' <= c && c <= '9') false
    else if ('a' <= c && c <= 'z') false
    else if ('A' <= c && c <= 'Z') false
    else if (ok contains c) false
    else true
  }

  def mangle(id: Id) = {
    val Id(name, index) = id
    if (name exists needsEscape) "|" + (name __ index) + "|"
    else name __ index
  }

  def sexpr(arg0: Any, args: Any*): String = {
    if (args.isEmpty) "(" + arg0 + ")"
    else "(" + arg0 + " " + args.mkString(" ") + ")"
  }

  def sexpr(args: Iterable[Any]): String = {
    args.mkString("(", " ", ")")
  }

  implicit class ProcessOps(process: Process) {
    def pid: Long = {
      val klass = process.getClass
      assert(klass.getName == "java.lang.UNIXProcess")
      val field = klass.getDeclaredField("pid");
      field.setAccessible(true)
      val res = field.getLong(process)
      field.setAccessible(false)
      res
    }
  }
}