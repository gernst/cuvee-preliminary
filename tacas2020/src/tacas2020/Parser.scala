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

  val expr: Parser[Expr] = P(id | parens(bind_ | app_ | wp_ | box_ | dia_))
  val exprs = P(expr +)

  val id = P(Id(name))
  val ids = id *
  val app_ = P(Apps(exprs))

  val forall = Forall("forall")
  val exists = Exists("exists")
  val quant = P(forall | exists)

  val formal = P(Formal(parens(id ~ typ)))
  val formals = P(formal *)
  val bind_ = P(Bind(quant ~ parens(formals) ~ expr))

  val prog: Parser[Prog] = P(parens(assign_ | spec_ | if_ | while_))
  val progs = prog *
  val block = P(Block(parens("block" ~ progs)))

  val wp_ = WP("wp" ~ block ~ expr)
  val box_ = WP("box" ~ block ~ expr)
  val dia_ = WP("dia" ~ block ~ expr)

  val let = parens(id ~ expr)
  val lets = let *
  val assign_ = Assign("assign" ~ lets)

  val spec_ = Spec("spec" ~ parens(ids) ~ expr ~ expr)
  val if_ = If("if" ~ expr ~ block ~ block)

  val term = ":termination" ~ expr
  val pre = ":precondition" ~ expr
  val post = ":postcondition" ~ expr
  val while_ = While("while" ~ expr ~ block ~ term ~ pre ~ post)

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