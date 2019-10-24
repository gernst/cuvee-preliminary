package tacas2020

import arse._
import arse.implicits._

object Parser {
  import scala.language.postfixOps
  implicit val whitespace = Whitespace.default

  def parens[A](p: Parser[A]) = {
    "(" ~ p ~ ")"
  }

  val name = S("[A-Za-z][A-Za-z0-9]+")

  val typ: Parser[Type] = P(sort)

  val sort = P(Sort(name))

  val expr: Parser[Expr] = P(id | parens(bind_ | app_))
  val exprs = P(expr +)

  val id = P(Id(name))
  val app_ = P(Apps(exprs))

  val forall = Forall("forall")
  val exists = Exists("exists")
  val quant = P(forall | exists)

  val formal = P(Formal("(" ~ id ~ typ ~ ")"))
  val formals = P(formal *)
  val bind_ = P(Bind(quant ~ "(" ~ formals ~ ")" ~ expr))

  val cmd: Parser[Cmd] = P(parens(exit_ | reset_ | push_ | pop_ | assert_ | get_assertions_))

  val exit_ = Exit("exit")
  val reset_ = Reset("reset")
  val push_ = Reset("push 1")
  val pop_ = Reset("pop 1")

  val assert_ = Assert("assert" ~ expr)
  
  val get_assertions_ = GetAssertions("get-assertions")

  val cmds = P(cmd *)
  val script = P(cmds $)
}