package cuvee

object Printer {
  def solver(solver: Solver) = solver.log.mkString("\n")

  def setLogic(logic: String) = sexpr("set-logic", logic)
  def setOption(args: List[String]) = sexpr("set-option", args: _*)

  def reset() = sexpr("reset")
  def push() = sexpr("push", 1)
  def pop() = sexpr("pop", 1)
  def exit() = sexpr("exit")

  def check() = sexpr("check-sat")
  def assertions() = sexpr("get-assertions")
  def model() = sexpr("get-model")

  def assert(expr: Expr) = sexpr("assert", expr)

  def id(id: Id) = mangle(id)

  def declare(sort: Sort, arity: Int) = {
    sexpr("declare-sort", sort, arity)
  }

  def define(sort: Sort, args: List[Sort], body: Type) = {
    sexpr("define-sort", sort, sexpr(args), body)
  }

  def declare(id: Id, args: List[Type], res: Type) = {
    sexpr("declare-fun", id, sexpr(args), res)
  }

  def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean) = {
    if (!rec) sexpr("define-fun", id, sexpr(formals), res, body)
    else sexpr("define-fun-rec", id, sexpr(formals), res, body)
  }
  
  def proc(in: List[Formal], out: List[Formal], pre: Expr, post: Expr, body: Prog) = {
    
  }

  def define(id: Id, proc: Proc) = {
    val Proc(in, out, pre, post, body) = proc
    sexpr("define-proc", id, sexpr(in), sexpr(out), body, ":precondition", pre, ":postcondition", post)
  }

  def define(sort: Sort, obj: Obj) = {
    val Obj(state, init, ops) = obj
    sexpr("define-class", sort :: sexpr(state) :: sexpr("init", init) :: (ops map sexpr2) : _*)
  }

  def sel(id: Id, typ: Type) = {
    sexpr(id, typ)
  }

  def constr(id: Id, sels: List[Sel]) = {
    sexpr(id, sels: _*)
  }

  def arity(sort: Sort, arity: Int) = {
    sexpr(sort, arity)
  }

  def datatype(params: List[Sort], constrs: List[Constr]) = {
    if (params.isEmpty) sexpr(constrs)
    else sexpr("par", sexpr(params), sexpr(constrs))
  }

  def declare(arities: List[Arity], decls: List[Datatype]) = {
    sexpr("declare-datatypes", sexpr(arities), sexpr(decls))
  }

  def sat() = "sat"
  def unsat() = "unsat"
  def unknown() = "unknown"

  def assertions(exprs: List[Expr]) = {
    sexpr(exprs)
  }

  def model(defs: List[Def]) = {
    sexpr("model", sexpr(defs))
  }

  def error(info: Seq[Any]) = {
    info.mkString("(error \"", ", ", "\")")
  }
}

object PrettyPrinter {
  def assert(expr: Expr): String = s"assert { ${printExpr(expr)} }"

  def define(id: Id, in: List[Formal], out: List[Formal], body: Prog, pre: Expr, post: Expr): String = {
    val in_ = in.map(v => (mangle(v.id), v.typ.toString))
    val out_ = out.map(v => (mangle(v.id), v.typ.toString))
    PrettyDefineProc(mangle(id), in_, out_, PrettyProg(body), PrettyExpr(pre), PrettyExpr(post)).print mkString "\n"
  }

  def printExpr(expr: Expr): String = PrettyExpr(expr).print mkString "\n"

  def printProg(p: Prog): String = PrettyProg(p).print mkString "\n"

  def printFormal(formal: Formal): String = s"${formal.id}: ${formal.typ}"

  /**
   * Joins a list of strings introducing line breaks and indention if necessary.
   *
   * @param parts  a list of strings. Each string is represented as a list of lines.
   * @param joiner the string which joins any two strings from the list
   * @param max    maximum line length
   * @return a list of lines
   */
  def breakIfNecessary(parts: List[List[String]], joiner: String = "", max: Int = 80): List[String] = {
    val (j, l) = if (joiner startsWith " ") {
      (joiner.substring(1), " ")
    } else {
      (joiner, "")
    }

    if (parts.isEmpty) {
      return List()
    }
    var indent = false
    var builder: List[String] = indentAllButFirst(parts.head)
    for (part <- parts drop 1 map indentAllButFirst) {
      part match {
        case first :: tail =>
          val lastLineLength = builder.last.length
          val firstNewLineLength = (j + first).length
          if (indent || lastLineLength + firstNewLineLength > max || tail.nonEmpty) {
            indent = true
          }
          builder = if (indent && !first.eq("{")) {
            l match {
              case " " => builder ++ (("  " + j + first) :: tail)
              case _ => builder.updated(builder.size - 1, builder.last + j) ++ (("  " + first) :: tail)
            }
          } else {
            builder.updated(builder.size - 1, builder.last + l + j + first) ++ tail
          }
      }
    }
    builder
  }

  def indentAllButFirst(lines: List[String]): List[String] = {
    lines match {
      case Nil => Nil
      case head :: tail => head :: tail.map { case "}" => "}" case other => "  " + other }
    }
  }

  def surround(pre: String, args: List[String], post: String): List[String] = {
    args match {
      case Nil => List(pre + post)
      case head :: Nil => List(pre + head + post)
      case head :: tail => (pre match {
        case "" => head
        case any => any + head
      }) :: surround("", tail, post)
    }
  }

  sealed trait PrettyExpr {
    /**
     * Prints this expression.
     *
     * @return a list of lines
     */
    def print: List[String]

    /**
     * @return a lower number binds first
     */
    def precedence: Int

    /**
     * Prints a sub expression surrounding it with parentheses if necessary
     */
    def printSub(prettyExpr: PrettyExpr): List[String] = {
      // this ignores associativity and might add redundant parentheses in that case
      if (prettyExpr.precedence >= precedence) {
        surround("(", prettyExpr.print, ")")
      } else {
        prettyExpr.print
      }
    }
  }

  object PrettyExpr {
    def apply(expr: Expr): PrettyExpr = expr match {
      case id: Id => PrettyId(mangle(id))
      case Num(value) => PrettyId(value toString)

      case Let(pairs) => ???
      case Match(expr, cases) => ???

      // ordered by precedence except where undefined or wildcard (e.g. general function application)

      case Select(array, index) => FunctionApplication(PrettyExpr(array), List(PrettyExpr(index)), ("[", "]")) // 1
      case Store(array, index, value) => FunctionApplication(PrettyExpr(array), List(PrettyExpr(index), PrettyExpr(value)), ("[", "]"), " ↦ ") // 1
      case Old(expr) => FunctionApplication(PrettyId("old"), List(apply(expr))) // 1

      case App(Id("not", None), List(arg)) => UnaryOperator("¬", apply(arg)) // 2
      case App(Id("-", None), List(arg)) => UnaryOperator("-", apply(arg)) // 2

      case App(Id("*", None), args) => BinaryOperator("*", args map apply, 5)
      case App(Id("/", None), args) => BinaryOperator("/", args map apply, 5)

      case App(Id("-", None), args) if args.size > 1 => BinaryOperator("-", args map apply, 6)
      case App(Id("+", None), args) if args.size > 1 => BinaryOperator("+", args map apply, 6)

      case App(Id("<=", None), args) => BinaryOperator("≤", args map apply, 8)
      case App(Id(">=", None), args) => BinaryOperator("≥", args map apply, 8)
      case App(Id("<", None), args) => BinaryOperator("<", args map apply, 8)
      case App(Id(">", None), args) => BinaryOperator(">", args map apply, 8)

      case Eq(left, right) => BinaryOperator("=", List(left, right) map apply, 9)
      case Distinct(List(left, right)) => BinaryOperator("≠", List(left, right) map apply, 9)

      case App(Id("and", None), args) => BinaryOperator("∧", args map apply, 11)
      case App(Id("or", None), args) => BinaryOperator("∨", args map apply, 12)

      case Ite(test, left, right) => PrettyIte(PrettyExpr(test), PrettyExpr(left), PrettyExpr(right)) // 13

      case App(Id("=>", None), args) if args.size == 2 => BinaryOperator("⟹", args map apply, 17)

      case App(fun, args) => FunctionApplication(PrettyId(mangle(fun)), args map apply) // 1

      case Bind(Forall, formals, body) => Binder("∀", formals map printFormal, apply(body))
      case Bind(Exists, formals, body) => Binder("∃", formals map printFormal, apply(body))

      case WP(prog, post) => ???
      case Box(prog, post) => ???
      case Dia(prog, post) => ???
    }

    case class PrettyIte(test: PrettyExpr, thenn: PrettyExpr, ellse: PrettyExpr) extends PrettyExpr {
      override def print: List[String] = {
        breakIfNecessary(List(printSub(test), surround(" ? ", printSub(thenn), ""), surround(" : ", printSub(ellse), "")))
      }

      override def precedence: Int = 13
    }
  }

  case class PrettyId(name: String) extends PrettyExpr {
    override def print: List[String] = List(name)

    override def precedence: Int = 0
  }

  case class FunctionApplication(name: PrettyExpr, args: List[PrettyExpr], parentheses: (String, String) = ("(", ")"), joiner: String = ", ") extends PrettyExpr {
    override def print: List[String] = breakIfNecessary(List(printSub(name), surround(parentheses._1, breakIfNecessary(args.map(_.print), joiner), parentheses._2)))

    override def precedence: Int = 1
  }

  case class UnaryOperator(symbol: String, arg: PrettyExpr) extends PrettyExpr {
    override def print: List[String] = surround(symbol, breakIfNecessary(List(printSub(arg))), "")

    override def precedence: Int = 2
  }

  case class BinaryOperator(symbol: String, args: List[PrettyExpr], prec: Int) extends PrettyExpr {
    override def print: List[String] = breakIfNecessary(args map printSub, s" $symbol ")

    override def precedence: Int = prec
  }

  case class Binder(symbol: String, formals: List[String], body: PrettyExpr) extends PrettyExpr {
    override def print: List[String] = surround(s"$symbol ", breakIfNecessary(List(surround("", breakIfNecessary(formals.map(List(_)), ", "), "."), printSub(body)), " "), "")

    override def precedence: Int = 20
  }

  sealed trait PrettyProg {
    /**
     * Prints this program fragment.
     *
     * @return a list of lines
     */
    def print: List[String]
  }

  object PrettyProg {
    def apply(prog: Prog): PrettyProg = prog match {
      case Block(progs, withOld) => PrettyBlock(progs map apply)
      case Break => ???
      case Assign(pairs) => PrettyAssign(pairs.map(pair => (mangle(pair.x), PrettyExpr(pair.e))))
      case Spec(xs, pre, post) => ???
      case If(test, left, Skip) => PrettyIf(PrettyExpr(test), PrettyProg(left), None)
      case If(test, left, right) => PrettyIf(PrettyExpr(test), PrettyProg(left), Some(PrettyProg(right)))
      case While(test, body, after, term, pre, post) => PrettyWhile(PrettyExpr(test), PrettyProg(body), PrettyProg(after), PrettyExpr(term), PrettyExpr(pre), PrettyExpr(post))
      case Call(name, in, out) => PrettyCall(mangle(name), in.map(PrettyExpr(_)), out.map(mangle))
    }
  }

  case class PrettyBlock(progs: List[PrettyProg]) extends PrettyProg {
    override def print: List[String] = progs match {
      case Nil => List("{ }")
      case list => List(List("{"), semicolon(list.map(prog => breakIfNecessary(List(prog.print)))), List("}")).flatten
    }
  }

  def semicolon(progs: List[List[String]]): List[String] = progs match {
    case Nil => Nil
    case head :: Nil => head
    case head :: tail => surround("", head, ";") ++ semicolon(tail)
  }

  case class PrettyAssign(pairs: List[(String, PrettyExpr)]) extends PrettyProg {
    override def print: List[String] = breakIfNecessary(List(
      breakIfNecessary(pairs.map(_._1).map(List(_)), ", "),
      breakIfNecessary(pairs.map(_._2).map(_.print), ", ")), " := ")
  }

  case class PrettyIf(test: PrettyExpr, positive: PrettyProg, negative: Option[PrettyProg]) extends PrettyProg {
    override def print: List[String] = {
      val rest = negative.map(prog => breakIfNecessary(List(List("else"), prog.print), " ")).getOrElse(Nil)
      breakIfNecessary(List(surround("if (", test.print, ")"), positive.print), " ") ++ rest
    }
  }

  case class PrettyWhile(test: PrettyExpr, body: PrettyProg, after: PrettyProg, term: PrettyExpr, pre: PrettyExpr, post: PrettyExpr) extends PrettyProg {
    override def print: List[String] = {
      val pre_ = surround("precondition { ", breakIfNecessary(List(pre print)), " }")
      val test_ = surround("while (", breakIfNecessary(List(test print)), ")")
      val term_ = surround("termination { ", breakIfNecessary(List(term print)), " }")
      val body_ = breakIfNecessary(List(body print))
      val while_ = breakIfNecessary(List(test_, body_), " ")
      val after_ = breakIfNecessary(List(after print))
      val post_ = surround("postcondition { ", breakIfNecessary(List(post print)), " }")
      pre_ ++ term_ ++ while_ ++ after_ ++ post_
    }
  }

  case class PrettyCall(symbol: String, in: List[PrettyExpr], out: List[String]) extends PrettyProg {
    override def print: List[String] = {
      val in_ = surround(s"$symbol(", breakIfNecessary(in.map(_.print), ", "), ")")
      val out_ = breakIfNecessary(List(out), ", ")
      breakIfNecessary(List(out_, in_), " := ")
    }
  }

  case class PrettyDefineProc(id: String, in: List[(String, String)], out: List[(String, String)], body: PrettyProg, pre: PrettyExpr, post: PrettyExpr) {
    def print: List[String] = {
      val pre_ = surround("precondition { ", breakIfNecessary(List(pre print)), " }")
      val in_ = surround(s"define-proc $id(", breakIfNecessary(in.map(v => List(s"${v._1}: ${v._2}")), ", "), "):")
      val out_ = surround("(", breakIfNecessary(out.map(v => List(s"${v._1}: ${v._2}")), ", "), ") =")
      val body_ = breakIfNecessary(List(body print))
      val proc_ = breakIfNecessary(List(in_, out_, body_), " ")
      val post_ = surround("postcondition { ", breakIfNecessary(List(post print)), " }")
      pre_ ++ proc_ ++ post_
    }
  }

}
