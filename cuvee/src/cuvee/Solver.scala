package cuvee

import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.File

trait Solver {
  var rlog: List[Cmd] = Nil
  def log = rlog.reverse

  def setLogic(logic: String): Ack
  def setOption(args: List[String]): Ack

  def reset(): Ack
  def push(): Ack
  def pop(): Ack
  def exit(): Ack

  def scoped[A](f: => A) = {
    push()
    try { f }
    finally { pop() }
  }

  def check(phi: Expr): IsSat = scoped {
    assert(phi)
    check()
  }

  def isSat(phi: Expr) = {
    val sat = check(phi)
    sat == Sat
  }

  def isUnsat(phi: Expr) = {
    val sat = check(phi)
    sat == Unsat
  }

  def check(): IsSat
  def assertions(): Assertions
  def model(): Model

  def declare(sort: Sort, arity: Int): Ack
  def define(sort: Sort, args: List[Sort], body: Type): Ack

  def declare(formal: Formal): Ack = declare(formal.id, formal.typ)
  def declare(id: Id, res: Type): Ack = declare(id, Nil, res)
  def declare(id: Id, args: List[Type], res: Type): Ack
  def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean): Ack

  def declare(arities: List[Arity], decls: List[Datatype]): Ack

  /**
   * Defines a procedure
   */
  def define(id: Id, in: List[Formal], out: List[Formal], body: Prog, pre: Expr, post: Expr): Ack

  def assert(expr: Expr): Ack

  def setOption(args: String*): Ack = {
    setOption(args.toList)
  }

  def exec(line: String): Option[Res] = {
    exec(Cmd.from(line))
  }

  def exec(cmd: Cmd): Option[Res] = {
    rlog = cmd :: rlog

    cmd match {
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
      case DefineProc(id, in, out, body, pre, post) =>
        Some(define(id, in, out, body, pre, post))
      case DeclareDatatypes(arity, decls) =>
        Some(declare(arity, decls))

      case Assert(expr) =>
        Some(assert(expr))
    }
  }
}

object Solver {
  def z3(timeout: Int = 1000) = process("z3", "-t:" + timeout, "-in")
  def cvc4(timeout: Int = 1000) = process("cvc4", "--tlimit=" + timeout, "--lang=smt2", "--incremental", "--increment-triggers")
  def princess(timeout: Int = 1000) = process("princess", "+stdin", "+quiet", "-timeoutPer=" + timeout, "+incremental")

  var traffic = false

  case class process(args: String*) extends Solver {
    val pb = new ProcessBuilder(args: _*)
    val pr = pb.start()
    val pid = pr.pid
    val stdout = new BufferedReader(new InputStreamReader(pr.getInputStream))
    val stdin = new PrintStream(pr.getOutputStream)

    if (traffic) println(args.mkString("$ ", " ", "") + " # " + pid)

    ensure(setOption(":print-success", "true") == Success)

    def setLogic(logic: String) = {
      write(Printer.setLogic(logic))
      Ack.from(read())
    }

    def setOption(args: List[String]) = {
      write(Printer.setOption(args))
      Ack.from(read())
    }

    def reset() = {
      write(Printer.reset())
      Ack.from(read())
    }

    def push() = {
      write(Printer.push())
      Ack.from(read())
    }

    def pop() = {
      write(Printer.pop())
      Ack.from(read())
    }

    def exit() = {
      write(Printer.exit())
      Ack.from(read())
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

    override def define(id: Id, in: List[Formal], out: List[Formal], body: Prog, pre: Expr, post: Expr) = {
      Success
    }

    def declare(arities: List[Arity], decls: List[Datatype]) = {
      write(Printer.declare(arities, decls))
      Ack.from(read())
    }

    def write(line: String) {
      if (traffic) println(pid + " < " + line)
      stdin.println(line)
      stdin.flush()
    }

    def read() = {
      val line = stdout.readLine()
      if (traffic) println(pid + " > " + line)
      line
    }
  }

  def file(out: File) = {
    val stream = new PrintStream(out)
    print(stream)
  }

  val stdout = print(System.out)

  case class print(stream: PrintStream) extends Solver {
    var sat: IsSat = Unknown

    def setLogic(logic: String) = {
      write(Printer.setLogic(logic))
      Success
    }

    def setOption(args: List[String]) = {
      write(Printer.setOption(args))
      Success
    }

    def reset() = {
      write(Printer.reset())
      Success
    }

    def push() = {
      write(Printer.push())
      Success
    }

    def pop() = {
      sat = Unknown
      write(Printer.pop())
      Success
    }

    def exit() = {
      write(Printer.exit())
      Success
    }

    def check() = {
      write(Printer.check())
      sat
    }

    def assert(expr: Expr) = {
      if (expr == False) sat = Unsat
      write(PrettyPrinter.assert(expr))
      Success
    }

    def assertions() = {
      write(Printer.assertions)
      Assertions(Nil)
    }

    def model() = {
      write(Printer.model())
      Model(Nil)
    }

    def declare(sort: Sort, arity: Int) = {
      write(Printer.declare(sort, arity))
      Success
    }

    def define(sort: Sort, args: List[Sort], body: Type) = {
      write(Printer.define(sort, args, body))
      Success
    }

    def declare(id: Id, args: List[Type], res: Type) = {
      write(Printer.declare(id, args, res))
      Success
    }

    def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean) = {
      write(Printer.define(id, formals, res, body, rec))
      Success
    }

    override def define(id: Id, in: List[Formal], out: List[Formal], body: Prog, pre: Expr, post: Expr) = {
      write(PrettyPrinter.define(id, in, out, body, pre, post))
      Success
    }

    def declare(arities: List[Arity], decls: List[Datatype]) = {
      write(Printer.declare(arities, decls))
      Success
    }

    def write(line: String) {
      stream.println(line)
      stream.flush()
    }
  }
}

