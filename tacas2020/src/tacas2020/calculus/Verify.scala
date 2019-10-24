package tacas2020.caclulus

import tacas2020.Error
import tacas2020.pure._
import tacas2020.syntax._

object Verify {
  import Eval._

  def prove(phi: Pure, st: State, what: String) = {
    val _phi = st.simplify(phi)
    val solver = st.solver
    solver.scoped {
      solver assume st.path
      if (!solver.isValid(_phi)) {
        throw Error("incorrect", what, _phi, st)
      }
    }
  }

  def verify(ids: (Id, Type)*)(funs: (Id, Fun)*)(pre: Expr, block: Block, post: Expr, what: String): Boolean = {
    val st0 = State.default

    val st1 = st0 declare ids
    val (_, st2) = st1 havoc (ids map (_._1))
    val st3 = st2 define funs
    val _pre = eval(pre, st3)

    try {
      print(what + " ... ")
      verify(st3 && _pre, block.open, post, what)
      println("success ❤")
      true
    } catch {
      case Error(msg, info @ _*) =>
        println(msg + " ⚡")
        for (more <- info) {
          println("  " + more)
        }
        false
    }
  }

  def verify(st: State, progs: List[Prog], post: Expr, what: String): Unit = {
    /* println(what)
    var _st: Option[State] = Some(st)
    var _indent = "  "

    while (!_st.isEmpty) {
      val Some(st) = _st
      println(_indent + "vars:  " + st.vars.mkString(", "))
      println(_indent + "store: " + st.store.mkString(", "))
      println(_indent + "path:  " + st.path.mkString(" && "))
      _st = st.old
      _indent += "  "
    } */

    progs match {
      case Nil =>
        /* println("  prove " + post)
        println() */
        verify(st, post, what)
      case first :: rest =>
        /* println("  execute " + first)
        println() */
        verify(st, first, rest, post, what)
    }
  }

  def verify(st: State, post: Expr, what: String): Unit = {
    val _post = eval(post, st)
    prove(_post, st, what)
  }

  def measure(test: Expr, term: Option[Expr]): Expr = term match {
    case None => tacas2020.syntax.True
    case Some(term) => test ==> ((0 <= term) && term < Old(term))
  }

  def verify(st0: State, first: Prog, rest: List[Prog], post: Expr, what: String): Unit = first match {
    case Assign(xs, es) =>
      val _es = es map (eval(_, st0))
      val st1 = st0 assign (xs, _es)
      verify(st1, rest, post, what)

    case let @ Let(ps, es) =>
      val xs = let.xs
      val _es = es map (eval(_, st0))
      val st1 = st0 params ps
      val st2 = st0 assign (xs, _es)
      verify(st2, rest, post, what)

    case Spec(phi, mod, psi) =>
      val _pre = eval(phi, st0)
      val (_, st1) = st0.mark havoc mod
      val _post = eval(psi, st1)
      val st2 = st1 && _post
      prove(_pre, st0, "spec pre")
      verify(st2, rest, post, what)

    case If(test, left, right) =>
      val _test = eval(test, st0)
      val st1 = st0 && _test
      val st2 = st0 && !_test
      verify(st1, left.open ++ rest, post, what)
      verify(st2, right.open ++ rest, post, what)

    case WhileInvariant(inv, term, While(test, body)) =>
      val st0_ = st0.mark
      val (_, st1_) = st0_ havoc body.mod

      val _inv = eval(inv, st1_)
      val _test = eval(test, st1_)
      val st2_ = st1_ && _inv && _test

      val decrease = measure(test, term)
      val spec = Spec(inv, body.mod, !test && inv)

      verify(st0_, spec :: rest, post, what)
      verify(st2_, body.open, decrease && inv, "loop body")

    case WhileContract(phi, term, While(test, body), second, psi, proof) =>
      val (_, st1) = st0 havoc body.mod
      val st0_ = st0.mark
      val st1_ = st1.mark

      val _test = eval(test, st1)
      val _pre = eval(phi, st1_)
      val st2_ = st1_ && !_test && _pre
      val st3_ = st1_ && _test && _pre

      val decrease = measure(test, term)
      val spec = Spec(phi, body.mod ++ second.mod, !test && psi)
      val indHyp = Spec(decrease && phi, body.mod ++ second.mod, !test && psi)

      verify(st0_, spec :: rest, post, what)
      verify(st2_, second.open, psi, "loop base")
      verify(st3_, body.open ++ List(indHyp) ++ proof.open, psi, "loop step")
  }
}