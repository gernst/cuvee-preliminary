package cuvee

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

  val typ: Parser[Type] = P(sort | parens(array_ | list_))
  val types = typ *

  val sort = P(Sort(name))
  val array_ = P(Type.array("Array" ~ typ ~ typ))
  val list_ = P(Type.list("List" ~ typ))

  val expr: Parser[Expr] = P(id | num | parens(bind_ | imp_ | and_ | eq_ | ite_ | select_ | store_ | old_ | wp_ | box_ | dia_ | app_))
  val exprs = P(expr +)

  val id = P(Id(name | op))
  val ids = P(id *)

  val num = P(Num(bigint))

  val old_ = P(Old("old" ~ expr))
  val imp_ = P(Imp("=>" ~ expr ~ expr)) // singled out to avoid clash with "="
  val and_ = P(And("and" ~ expr.*))
  val eq_ = P(Eq("=" ~ expr ~ expr))
  val ite_ = P(Ite("ite" ~ expr ~ expr ~ expr))

  val select_ = P(Select("select" ~ expr ~ expr))
  val store_ = P(Store("store" ~ expr ~ expr ~ expr))

  val app_ = P(Apps(exprs))

  val forall = Forall("forall")
  val exists = Exists("exists")
  val quant = P(forall | exists)

  val formal = P(Formal(parens(id ~ typ)))
  val formals = P(formal *)
  val bind_ = P(Bind(quant ~ parens(formals) ~ expr))

  val prog: Parser[Prog] = P(parens(break_ | assign_ | asm_ | asrt_ | spec_ | if_ | while_ | block_))
  val progs = P(prog *)
  val block_ = P(Block("block" ~ progs))

  val wp_ = P(WP("wp" ~ prog ~ expr))
  val box_ = P(Box("box" ~ prog ~ expr))
  val dia_ = P(Dia("dia" ~ prog ~ expr))
  
  val let = P(Let(parens(id ~ expr)))
  val lets = P(let *)
  val assign_ = P(Assign("assign" ~ lets))

  val break_ = P(Break("break"))
  val asm_ = P(Spec.assume("assume" ~ expr))
  val asrt_ = P(Spec.assert("assert" ~ expr))
  val spec_ = P(Spec("spec" ~ parens(ids) ~ expr ~ expr))
  val if_ = P(If("if" ~ expr ~ prog ~ prog.?))

  val term = P(":termination" ~ expr)
  val pre = P(":precondition" ~ expr)
  val post = P(":postcondition" ~ expr)
  val while_ = P(While("while" ~ expr ~ prog ~ prog.? ~ term.? ~ pre.? ~ post.?))

  val cmd: Parser[Cmd] = P(parens(set_logic_ | exit_ | reset_ | push_ | pop_ | check_sat_ | verify_ | assert_ | get_model_ | get_assertions_ |
    declare_sort_ | declare_const_ | declare_fun_ | define_fun_rec_ | define_fun_))

  val set_logic_ = P(SetLogic("set-logic" ~ name))
  val get_model_ = P(GetModel("get-model"))
  val exit_ = P(Exit("exit"))
  val reset_ = P(Reset("reset"))
  val push_ = P(Push("push"))
  val pop_ = P(Pop("pop"))

  val check_sat_ = P(CheckSat("check-sat"))

  val assert_ = P(Assert("assert" ~ expr))
  val verify_ = P(CounterExample("assert-counterexample" ~ expr ~ prog ~ expr))

  val get_assertions_ = P(GetAssertions("get-assertions"))

  val declare_sort_ = P(DeclareSort("declare-sort" ~ sort ~ int))
  val declare_const_ = P(DeclareFun("declare-const" ~ id ~ ret(Nil) ~ typ))
  val declare_fun_ = P(DeclareFun("declare-fun" ~ id ~ parens(types) ~ typ))
  val define_fun_ = P(DefineFun("define-fun" ~ id ~ parens(formals) ~ typ ~ expr))
  val define_fun_rec_ = P(DefineFunRec("define-fun-rec" ~ id ~ parens(formals) ~ typ ~ expr))

  val cmds = P(cmd *)
  val script = P(cmds $)
}