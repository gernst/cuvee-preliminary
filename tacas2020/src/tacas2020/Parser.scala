package tacas2020

import arse._
import arse.implicits._

object Parser {
  import scala.language.postfixOps
  implicit val whitespace = Whitespace.default

  def parens[A](p: Parser[A]) = {
    "(" ~ p ~ ")"
  }

  val name = S("[A-Za-z_][A-Za-z0-9_]*")
  val op = L("-") | L("+") | L("-") | L("<=") | L("<") | L(">=") | L(">")

  val typ: Parser[Type] = P(sort)
  val types = typ *

  val sort = P(Sort(name))

  val expr: Parser[Expr] = P(id | num | parens(bind_ | imp_ | eq_ | ite_ | old_ | wp_ | box_ | dia_ | app_))
  val exprs = P(expr +)

  val id = P(Id(name | op))
  val ids = P(id *)

  val num = P(Num(bigint))

  val old_ = P(Old("old" ~ expr))
  val imp_ = P(Imp("=>" ~ expr ~ expr))
  val eq_ = P(Eq("=" ~ expr ~ expr))
  val ite_ = P(Ite("ite" ~ expr ~ expr ~ expr))

  val app_ = P(Apps(exprs))

  val forall = Forall("forall")
  val exists = Exists("exists")
  val quant = P(forall | exists)

  val formal = P(Formal(parens(id ~ typ)))
  val formals = P(formal *)
  val bind_ = P(Bind(quant ~ parens(formals) ~ expr))

  val prog: Parser[Prog] = P(parens(assign_ | spec_ | if_ | while_))
  val progs = P(prog *)
  val block = P(Block(parens("block" ~ progs)))

  val wp_ = P(WP("wp" ~ block ~ expr))
  val box_ = P(Box("box" ~ block ~ expr))
  val dia_ = P(Dia("dia" ~ block ~ expr))

  val let = P(Let(parens(id ~ expr)))
  val lets = P(let *)
  val assign_ = P(Assign("assign" ~ lets))

  val spec_ = P(Spec("spec" ~ parens(ids) ~ expr ~ expr))
  val if_ = P(If("if" ~ expr ~ block ~ block))

  val term = P(":termination" ~ expr)
  val pre = P(":precondition" ~ expr)
  val post = P(":postcondition" ~ expr)
  val while_ = P(While("while" ~ expr ~ block ~ term ~ pre ~ post))

  val cmd: Parser[Cmd] = P(parens(exit_ | reset_ | push_ | pop_ | assert_ | get_assertions_ | declare_const_ | declare_fun_))

  val exit_ = P(Exit("exit"))
  val reset_ = P(Reset("reset"))
  val push_ = P(Reset("push 1"))
  val pop_ = P(Reset("pop 1"))

  val assert_ = P(Assert("assert" ~ expr))

  val get_assertions_ = P(GetAssertions("get-assertions"))

  val declare_const_ = P(DeclareFun("declare-const" ~ id ~ ret(Nil) ~ typ))
  val declare_fun_ = P(DeclareFun("declare-fun" ~ id ~ parens(types) ~ typ))

  val cmds = P(cmd *)
  val script = P(cmds $)
}