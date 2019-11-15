package cuvee

import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader

trait Solver {
  def setLogic(logic: String): Ack
  def setOption(args: List[String]): Ack

  def reset(): Unit
  def push(): Unit
  def pop(): Unit
  def exit(): Unit

  def check(): IsSat
  def assertions(): Assertions
  def model(): Model

  def declare(sort: Sort, arity: Int): Ack
  def define(sort: Sort, args: List[Sort], body: Type): Ack

  def declare(id: Id, args: List[Type], res: Type): Ack
  def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean): Ack

  def assert(expr: Expr): Ack
  
  def setOption(args: String*): Ack = {
    setOption(args.toList)
  }
  
  def exec(cmd: Cmd): Option[Res] = cmd match {
    case SetLogic(logic) =>
      Some(setLogic(logic))
    case SetOption(args) =>
      Some(setOption(args))

    case Reset =>
      reset(); None
    case Push =>
      push(); None
    case Pop =>
      pop(); None
    case Exit =>
      exit(); None

    case CheckSat =>
      Some(check())
    case GetAssertions =>
      Some(assertions())
    case GetModel =>
      Some(model())

    case DeclareSort(sort, arity) =>
      Some(declare(sort, arity))
    case DefineSort(sort, args, body) =>
      Some(define(sort, args, body))

    case DeclareFun(id, args, res) =>
      Some(declare(id, args, res))
    case DefineFun(id, formals, res, body) =>
      Some(define(id, formals, res, body, false))
    case DefineFunRec(id, formals, res, body) =>
      Some(define(id, formals, res, body, true))

    case Assert(expr) =>
      Some(assert(expr))
  }
}

object Solver {
  def z3(timeout: Int = 1000) = process("z3", "-t:" + timeout, "-in")
  def cvc4(timeout: Int = 1000) = process("cvc4", "--tlimit=" + timeout, "--lang=smt2", "--increment-triggers")

  case class process(args: String*) extends Solver {
    val pb = new ProcessBuilder(args: _*)
    val pr = pb.start()
    val stdout = new BufferedReader(new InputStreamReader(pr.getInputStream))
    val stdin = new PrintStream(pr.getOutputStream)

    ensure(setOption(":print-success", "true") == Success)

    def setLogic(logic: String) = {
      write(Printer.setLogic(logic))
      Ack.from(read())
    }

    def setOption(args: List[String]) = {
      write(Printer.setOption(args))
      Ack.from(read())
    }

    def reset() {
      write(Printer.reset())
    }

    def push() {
      write(Printer.push())
    }

    def pop() {
      write(Printer.pop())
    }

    def exit() {
      write(Printer.exit())
    }

    def check() = {
      write(Printer.check())
      IsSat.from(read())
    }

    def assert(expr: Expr) = {
      write(Printer.assert(expr))
      Ack.from(read())
    }

    def assertions() = {
      write(Printer.assertions)
      Assertions.from(read())
    }

    def model() = {
      write(Printer.model())
      Model.from(read())
    }

    def declare(sort: Sort, arity: Int) = {
      write(Printer.declare(sort, arity))
      Ack.from(read())
    }

    def define(sort: Sort, args: List[Sort], body: Type) = {
      write(Printer.define(sort, args, body))
      Ack.from(read())
    }

    def declare(id: Id, args: List[Type], res: Type) = {
      write(Printer.declare(id, args, res))
      Ack.from(read())
    }

    def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean) = {
      write(Printer.define(id, formals, res, body, rec))
      Ack.from(read())
    }

    def write(line: String) {
      stdin.println(line)
      stdin.flush()
    }

    def read() = {
      stdout.readLine()
    }
  }
}

