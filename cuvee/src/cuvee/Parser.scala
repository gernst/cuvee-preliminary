package cuvee

import arse._
import arse.implicits._
import java.io.File

class Parseable[+A](p: Parser[A]) {
  def from(text: String): A = {
    import Parser.whitespace
    ensure(text != null, "cannot parse the null string")
    // val withoutComments = text split raw"[\r\n]+" filter (!_.startsWith(";")) mkString "\n"
    // p.parseAll(withoutComments)
    p.parseAll(text)
  }

  def fromFile(path: String): A = {
    from(new File(path))
  }

  def from(file: File): A = {
    from(file.text)
  }
}

object Parser {
  implicit val whitespace: Whitespace = {
    new Whitespace("(\\s|(\\s*;.*(\\n|$)))*")
  }

  def parens[A](p: Parser[A]) = {
    "(" ~ p ~ ")"
  }

  // https://github.com/smtlib/jSMTLIB
  // SMT/src/org/smtlib/sexpr/Lexer.java
  val simple = S("[a-zA-Z_~!@$%^&*+=<>.?/\\-][0-9a-zA-Z_~!@$%^&*+=<>.?/\\-]*")
  val quoted = S("\\|[0-9a-zA-Z_~!@$%^&*+=<>.?/\"'(),:;{}#`\\[\\] \t\r\n\\-]*\\|") map {
    str => str.substring(1, str.length - 1)
  }

  val name = simple | quoted
  val kw = S(":[0-9a-zA-Z_~!@$%^&*+=<>.?/\\-]+")
  val op = L("-") | L("+") | L("<=") | L("<") | L(">=") | L(">")

  val typ: Parser[Type] = P(sort | parens(array_ | list_))
  val types = typ.*

  val sort = P(Sort(name))
  val array_ = P(Type.array("Array" ~ typ ~ typ))
  val list_ = P(Type.list("List" ~ typ))

  val pat: Parser[Pat] = P(id | parens(unapp_))
  val expr: Parser[Expr] = P(id | num | parens(as_ | bind_ | distinct_ | imp_ | and_ | or_ | eq_ | ite_ | let_ | match_ | select_ | store_ | old_ | wp_ | box_ | dia_ | app_))

  val id = P(Id(name | op))

  val num = P(Num(bigint))
  val pair = P(Pair(parens(id ~ expr)))
  val pairs = P(pair.*)

  val attr_ = P(Attr(kw ~ name.?))
  val note_ = P(Note(expr ~ attr_.+))

  val as_ = P(As("as" ~ id ~ sort))
  val old_ = P(Old("old" ~ expr))
  val imp_ = P(Imp("=>" ~ expr ~ expr)) // singled out to avoid clash with "="
  val and_ = P(And.nary("and" ~ expr.*))
  val or_ = P(Or.nary("or" ~ expr.*))
  val distinct_ = P(Distinct("distinct" ~ expr.*))
  val eq_ = P(Eq("=" ~ expr ~ expr))
  val ite_ = P(Ite("ite" ~ expr ~ expr ~ expr))
  val let_ = P(Let("let" ~ parens(pairs) ~ expr))

  val cs = P(parens(Case(pat ~ expr)))
  val match_ = P(Match("match" ~ expr ~ cs.*))

  val select_ = P(Select("select" ~ expr ~ expr))
  val store_ = P(Store("store" ~ expr ~ expr ~ expr))

  val app_ = P(Apps(expr.+))
  val unapp_ = P(UnApps(id.+))

  val forall = Forall("forall")
  val exists = Exists("exists")
  val quant = P(forall | exists)

  val formal = P(Formal(parens(id ~ typ)))
  val formals = P(formal.*)
  val bind_ = P(Bind(quant ~ parens(formals) ~ expr))

  val prog: Parser[Prog] = P(parens(break_ | assign_ | asm_ | asrt_ | spec_ | call_ | if_ | while_ | block_))
  val progs = P(prog.*)
  val block_ = P(Block("block" ~ progs))

  val wp_ = P(WP("wp" ~ prog ~ expr))
  val box_ = P(Box("box" ~ prog ~ expr))
  val dia_ = P(Dia("dia" ~ prog ~ expr))

  val assign_ = P(Assign("assign" ~ pairs))

  val break_ = P(Break("break"))
  val asm_ = P(Spec.assume("assume" ~ expr))
  val asrt_ = P(Spec.assert("assert" ~ expr))
  val spec_ = P(Spec("spec" ~ parens(id.*) ~ expr ~ expr))
  val call_ = P(Call("call" ~ id ~ parens(expr.*) ~ parens(id.*)))
  val if_ = P(If("if" ~ expr ~ prog ~ prog.?))

  val term = P(":termination" ~ expr)
  val pre = P(":precondition" ~ expr)
  val post = P(":postcondition" ~ expr)
  val while_ = P(While("while" ~ expr ~ prog ~ prog.? ~ term.? ~ pre.? ~ post.?))

  val cmd_ : Parser[Cmd] = P(set_logic_ | set_option_ | set_info_ | exit_ | reset_ | push_ | pop_ | check_sat_ | verify_ | assert_ | get_model_ | get_assertions_ |
    declare_sort_ | declare_const_ | declare_fun_ | define_fun_rec_ | define_fun_ | declare_dts_ |
    define_proc_ | define_class_ | verify_refinement_)

  val cmd: Parser[Cmd] = P(parens(cmd_))

  val set_logic_ = P(SetLogic("set-logic" ~ name))
  val set_option_ = P(SetOption("set-option" ~ (kw :: name.*)))
  val set_info_ = P(SetInfo("set-info" ~ kw ~ (string | name).?))
  val get_model_ = P(GetModel("get-model"))
  val exit_ = P(Exit("exit"))
  val reset_ = P(Reset("reset"))

  val success = P(Success("success"))
  val unsupported = P(Unsupported("unsupported"))
  val sat = P(Sat("sat"))
  val unsat = P(Unsat("unsat"))
  val unknown = P(Unknown("unknown"))
  val error = P(Error("error" ~ ret("unknown")))
  val error_ = P(Error("error" ~ string))

  // Note: explicit types prevent some complication weirdness about cyclic references
  val ack: Parser[Ack] = P(success | unsupported | error | parens(error_))
  val is_sat: Parser[IsSat] = P(sat | unsat | unknown)
  val res: Parser[Res] = P(ack | is_sat)

  val int_0 = int | ret(0)
  val int_1 = int | ret(1)

  val push_ = P(Push("push" ~ int_1))
  val pop_ = P(Pop("pop" ~ int_1))

  val expect_is_sat = ":expect" ~ is_sat
  val check_sat_ = P(CheckSat("check-sat" ~ expect_is_sat.?))

  val assert_ = P(Assert("assert" ~ expr))
  val verify_ = P(CounterExample("assert-counterexample" ~ expr ~ prog ~ expr))

  val get_assertions_ = P(GetAssertions("get-assertions"))

  val declare_sort_ = P(DeclareSort("declare-sort" ~ sort ~ int_0))
  val declare_const_ = P(DeclareFun("declare-const" ~ id ~ ret(Nil) ~ typ))
  val declare_fun_ = P(DeclareFun("declare-fun" ~ id ~ parens(types) ~ typ))
  val define_fun_ = P(DefineFun("define-fun" ~ id ~ parens(formals) ~ typ ~ expr))
  val define_fun_rec_ = P(DefineFunRec("define-fun-rec" ~ id ~ parens(formals) ~ typ ~ expr))

  val proc_ = Proc(parens(formals) ~ parens(formals) ~ prog ~ pre.? ~ post.?)
  val define_proc_ = P(DefineProc("define-proc" ~ id ~ proc_))

  val obj_init = parens("init" ~ proc_)
  val obj_op = parens(id ~ proc_)
  val obj_ = Obj(parens(formals) ~ obj_init ~ obj_op.*)
  val define_class_ = P(DefineClass("define-class" ~ sort ~ obj_))
  val define_refinement_ = P(DefineRefinement("refinement" ~ formal ~ formal ~ expr))

  val refine_by_fun_ = VerifyRefinement(sort ~ sort ~ Sim.byFun(id))
  val refine_by_expr_ = parens(sort ~ formals) ~ parens(sort ~ formals) ~ expr map {
    case (((spec, as), (impl, cs)), phi) =>
      VerifyRefinement(spec, impl, Sim.byExpr(as, cs, phi))
  }

  val verify_refinement_ = ("verify-refinement" ~ (refine_by_fun_ | refine_by_expr_))

  val sel = P(Sel(parens(id ~ typ)))
  val constr = P(parens(Constr(id ~ sel.*)))
  val datatype_ = P(Datatype("par" ~ parens(sort.+) ~ constr.+))
  val param_datatype_ = P(Datatype(ret(Nil) ~ constr.+))
  val datatype = parens(param_datatype_ | datatype_)
  val arity = P(Arity(parens(sort ~ int)))
  val declare_dts_ = P(DeclareDatatypes("declare-datatypes" ~ parens(arity.*) ~ parens(datatype.*)))

  val dfn_ = P(define_fun_ | define_fun_rec_)
  val dfn = parens(dfn_)

  val assertions_ = P(Assertions(expr.*))
  val assertions: Parser[Assertions] = parens(assertions_)

  val model_ = P(Model("model" ~ dfn.*))
  val model: Parser[Model] = parens(model_)

  val cmds = P(cmd.*)
  val script = P(cmds.$)
}