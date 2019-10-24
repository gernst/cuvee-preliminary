package tacas2020

import java.io.BufferedReader
import java.io.File
import java.io.FileWriter
import java.io.InputStreamReader
import java.io.PrintStream
import java.time.LocalDate
import java.util.Date
import java.text.SimpleDateFormat

object SMT2 {
  case class state(
    var sorts: Set[String],
    var funs: Set[String],
    var vars: Set[String],
    var rcmds: List[String]) {
    def cmds = rcmds.reverse
  }

  def z3(timeout: Int) = new SMT2("z3", "-t:" + timeout, "-in") {
    override def declare_list() {} // Z3 has builtin lists
  }

  def cvc4(timeout: Int) = new SMT2("cvc4", "--tlimit=" + timeout, "--lang=smt2", "--increment-triggers") {
    command("set-logic", "ALL")
  }

  val pointer = Sort.int // Sort.base("Pointer")
}

trait Backend {
  def write(line: String, state: SMT2.state): Unit
  def read(): String
  def check(state: SMT2.state): Unit
}

object Backend {
  class incremental(args: String*) extends Backend {
    val pb = new ProcessBuilder(args: _*)
    val pr = pb.start()
    val stdout = new BufferedReader(new InputStreamReader(pr.getInputStream))
    val stderr = pr.getErrorStream
    val stdin = new PrintStream(pr.getOutputStream)

    val log = new FileWriter(new File("log.smt2"))

    write("(set-option :print-success true)", null)
    read()

    def write(line: String, state: SMT2.state) {
      ok()
      // println("SMT > " + line)
      log.write(line)
      log.write("\n")
      log.flush()
      stdin.println(line)
      stdin.flush()
    }

    def check(state: SMT2.state) {
      write("(check-sat)", state)
    }

    def read(): String = {
      ok()
      val line = stdout.readLine.trim
      // println("SMT < " + line)
      line
    }

    def ok() {
      if (!pr.isAlive)
        throw ProofError("backend died", pr.exitValue)
    }
  }

  class oneshot(args: String*) extends Backend {
    var result: String = ""

    def write(line: String, state: SMT2.state): Unit = {
      result = "success"
    }

    def read(): String = {
      result
    }

    def check(state: SMT2.state): Unit = {
      val pb = new ProcessBuilder(args: _*)
      val pr = pb.start()
      val stdout = new BufferedReader(new InputStreamReader(pr.getInputStream))
      val stderr = pr.getErrorStream
      val stdin = new PrintStream(pr.getOutputStream)

      stdin.println(state.cmds.mkString("\n"))
      stdin.println("(check-sat)")
      stdin.flush()
      result = stdout.readLine.trim
      stdin.println("(quit)")
      stdin.flush()
      stdin.close()
      pr.waitFor()
    }
  }
}

class SMT2(args: String*) extends Solver {
  val backend: Backend = new Backend.incremental(args: _*)

  object State {
    val empty = SMT2.state(Set(), Set(), Set(), List())
  }

  var stack = List(State.empty)
  def state = stack.head
  state.rcmds = Nil // don't track this command commands

  // command("set-option", ":produce-assertions", "true")
  command("set-logic", "ALL")

  def push() = {
    stack = state.copy() :: stack
    command("push")
  }

  def pop() = {
    command("pop")
    stack = stack.tail
  }

  override def toString() = {
    state.cmds.mkString("\n")
  }

  def isConsistent: Boolean = {
    //    println()
    //    println(">>>>>>>>>>>>>>>>>>>>")
    //    println(this)
    //    println("<<<<<<<<<<<<<<<<<<<<")

    backend.check(state)
    val result = backend.read()

    result match {
      case "sat" =>
        true
      case "unsat" =>
        false
      case _ =>
        val today = new Date()
        val format = new SimpleDateFormat("yyyyMMdd-HHmmss")
        val time = format.format(today)
        val path = "timeout-" + time + ".smt2"
        val out = new FileWriter(new File(path))
        out.write(toString)
        out.write("\n")
        out.write("(check-sat)\n")
        out.flush()
        out.close()

        throw ProofUnknown("timeout", result, "re-run this query with", args.mkString("", " ", " < ") + path)
    }
  }

  def sexpr(arg0: String, args: String*) = {
    "(" + arg0 + " " + args.mkString(" ") + ")"
  }

  def sexpr(args: Iterable[String]) = {
    args mkString ("(", " ", ")")
  }

  def command(name: String, args: String*) = {
    val lengths = args map (_.length)
    val break = name.length + lengths.sum > 80 || !lengths.isEmpty && lengths.max > 20

    val line = new StringBuilder

    line append "("
    line append name
    for (arg <- args) {
      if (break)
        line append "\n  "
      else
        line append " "
      line append arg
    }
    line append ")"

    val cmd = line.toString
    state.rcmds = cmd :: state.rcmds

    backend.write(cmd, state)

    val out = backend.read()
    if (out != "success") {
      println(this)
      throw ProofError(cmd, out)
    }
  }

  def declare_sort(sort: Sort.base) {
    val Sort.base(name) = sort
    if (!(state.sorts contains name)) {
      state.sorts += name
      command("declare-sort", name, "0")
    }
  }

  def declare_pointer() {
    declare_typ(SMT2.pointer)
  }

  def declare_list() {
    val name = "List"
    if (!(state.sorts contains name)) {
      state.sorts += name
      command("declare-datatypes", "((" + name + " 1))", "((par (T) ((nil) (insert (head T) (tail (" + name + " T))))))")
      // command("declare-datatypes", "(T)", "((" + name + " nil (cons (head T) (tail " + name + "))))")
    }
  }

  def declare_fun(fun: Fun) {
    val name = mangle(fun.name.toString)
    val args = fun.args
    val ret = fun.ret
    if (!(state.funs contains name)) {
      state.funs += name
      command("declare-fun", name, sexpr(args map smt), smt(ret))
    }
  }

  def declare_var(name: String, typ: Sort) {
    if (!(state.vars contains name)) {
      state.vars += name
      command("declare-const", name, smt(typ))
    }
  }

  def declare_typ(typ: Sort): Unit = typ match {
    case Sort.bool =>
    case Sort.int =>
    case Sort.pointer(elem) =>
      declare_pointer()
      declare_typ(elem)
    case sort: Sort.base =>
      declare_sort(sort)
    case Sort.list(elem) =>
      declare_typ(elem)
      declare_list()
    case Sort.array(dom, ran) =>
      declare_typ(dom)
      declare_typ(ran)
  }

  def assumeDistinct(exprs: Iterable[Pure]) = {
    val args = smt(exprs)
    command("assert", sexpr("distinct", args: _*))
  }

  def assume(phi: Pure) {
    command("assert", smt(phi))
  }

  def assert(phi: Pure) {
    assume(!phi)
  }

  def smt(typ: Sort): String = typ match {
    case _: Param =>
      ???
    case Sort.bool =>
      "Bool"
    case Sort.int =>
      "Int"
    case ptr: Sort.pointer =>
      smt(SMT2.pointer)
    case sort @ Sort.base(name) =>
      declare_typ(typ)
      name
    case Sort.list(elem) =>
      declare_list()
      sexpr("List", smt(elem))
    case Sort.array(dom, ran) =>
      declare_typ(dom)
      declare_typ(ran)
      sexpr("Array", smt(dom), smt(ran))
  }

  def mangle(name: String): String = {
    if (name forall (_.isLetterOrDigit)) name
    else "|" + name + "|"
  }
  def mangle(name: String, index: Option[Int]): String = index match {
    case None => mangle(name)
    case Some(index) => mangle(name + index)
  }

  // Note: don't reuse smt(x) as it remembers the constant
  def bind(x: Var) = x match {
    case Var(name, typ, index) =>
      val id = mangle(name, index)
      sexpr(id, smt(typ))
  }

  def smt(exprs: Iterable[Pure]): Seq[String] = {
    smt(Set(), exprs)
  }

  def smt(expr: Pure): String = {
    smt(Set(), expr)
  }

  def smt(scope: Set[Var], exprs: Iterable[Pure]): Seq[String] = {
    val res = exprs map (smt(scope, _))
    res.toSeq
  }

  def smt(scope: Set[Var], expr: Pure): String = expr match {
    case x @ Var(name, typ, index) =>
      val id = mangle(name, index)
      if (!(scope contains x))
        declare_var(id, typ)
      id

    case App(fun, args) if (Solver.uninterpreted contains fun) =>
      declare_fun(fun)
      if (args.isEmpty)
        mangle(fun.name)
      else
        sexpr(mangle(fun.name), smt(scope, args): _*)

    case True =>
      "true"
    case False =>
      "false"

    case Const(name, Sort.int) if name.toString forall (_.isDigit) =>
      name.toString

    case Pure.ite(arg1, arg2, arg3) =>
      sexpr("ite", smt(scope, arg1), smt(scope, arg2), smt(scope, arg3))

    case Pure.times(arg1, arg2) =>
      sexpr("*", smt(scope, arg1), smt(scope, arg2))
    case Pure.mod(arg1, arg2) =>
      sexpr("mod", smt(scope, arg1), smt(scope, arg2))
    case Pure.divBy(arg1, arg2) =>
      sexpr("div", smt(scope, arg1), smt(scope, arg2))
    //    case Pure.exp(arg1, arg2) =>
    //      sexpr("exp", smt(scope, arg1), smt(scope, arg2))

    case Pure.uminus(arg) =>
      sexpr("-", smt(scope, arg))
    case Pure.plus(arg1, arg2) =>
      sexpr("+", smt(scope, arg1), smt(scope, arg2))
    case Pure.minus(arg1, arg2) =>
      sexpr("-", smt(scope, arg1), smt(scope, arg2))

    case Pure._eq(arg1, arg2) =>
      sexpr("=", smt(scope, arg1), smt(scope, arg2))
    case Pure.lt(arg1, arg2) =>
      sexpr("<", smt(scope, arg1), smt(scope, arg2))
    case Pure.le(arg1, arg2) =>
      sexpr("<=", smt(scope, arg1), smt(scope, arg2))
    case Pure.gt(arg1, arg2) =>
      sexpr(">", smt(scope, arg1), smt(scope, arg2))
    case Pure.ge(arg1, arg2) =>
      sexpr(">=", smt(scope, arg1), smt(scope, arg2))

    case Pure.not(arg) =>
      sexpr("not", smt(scope, arg))
    case Pure.and(arg1, arg2) =>
      sexpr("and", smt(scope, arg1), smt(scope, arg2))
    case Pure.or(arg1, arg2) =>
      sexpr("or", smt(scope, arg1), smt(scope, arg2))
    case Pure.imp(arg1, arg2) =>
      sexpr("=>", smt(scope, arg1), smt(scope, arg2))
    case Pure.eqv(arg1, arg2) =>
      sexpr("=", smt(scope, arg1), smt(scope, arg2))

    case App(Fun.nil, List()) =>
      "nil"
    case Pure.cons(arg1, arg2) =>
      sexpr("insert", smt(scope, arg1), smt(scope, arg2))
    case Pure.head(arg) =>
      sexpr("head", smt(scope, arg))
    case Pure.tail(arg) =>
      sexpr("tail", smt(scope, arg))

    case Pure.select(arg1, arg2) =>
      sexpr("select", smt(scope, arg1), smt(scope, arg2))
    case Pure.store(arg1, arg2, arg3) =>
      sexpr("store", smt(scope, arg1), smt(scope, arg2), smt(scope, arg3))

    case App(fun, args) =>
      declare_fun(fun)
      if (args.isEmpty)
        mangle(fun.name)
      else
        sexpr(mangle(fun.name), smt(scope, args): _*)

    case Bind(q, bound, body) =>
      sexpr(q.toString, sexpr(bound map bind), smt(scope ++ bound, body))
  }
}
