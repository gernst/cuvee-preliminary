package cuvee

import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.File
import java.io.ByteArrayOutputStream

trait Solver {
  var rlog: List[Cmd] = Nil // can't use generic C here
  def log = rlog.reverse

  def setLogic(logic: String): Ack
  def setOption(args: List[String]): Ack

  def reset(): Ack
  def push(depth: Int): Ack
  def pop(depth: Int): Ack
  def exit(): Ack

  def scoped[A](f: => A) = {
    push(1)
    try { f }
    finally { pop(1) }
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

  def bind(formals: List[Formal]) = {
    for (Formal(id, typ) <- formals) {
      declare(id, typ)
    }
  }

  def check(): IsSat

  def check(expected: IsSat): IsSat = {
    check()
  }

  def assertions(): Assertions
  def model(): Model

  def declare(sort: Sort, arity: Int): Ack
  def define(sort: Sort, args: List[Sort], body: Type): Ack

  def declare(formal: Formal): Ack = declare(formal.id, formal.typ)
  def declare(id: Id, res: Type): Ack = declare(id, Nil, res)
  def declare(id: Id, args: List[Type], res: Type): Ack
  def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean): Ack

  def declare(arities: List[Arity], decls: List[Datatype]): Ack

  def define(id: Id, proc: Proc): Ack
  def define(sort: Sort, obj: Obj): Ack

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
      case Push(depth) =>
        push(depth); None
      case Pop(depth) =>
        pop(depth); None
      case Exit =>
        exit(); None

      case CheckSat(None) =>
        Some(check())
      case CheckSat(Some(expected)) =>
        Some(check(expected))
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
      case DeclareDatatypes(arity, decls) =>
        Some(declare(arity, decls))

      case Assert(expr) =>
        Some(assert(expr))

      case DefineProc(id, proc) =>
        Some(define(id, proc))
      case DefineClass(sort, obj) =>
        Some(define(sort, obj))

      case _ => Some(Error("not supported"))
    }
  }
}

object Solver {
  def default = z3()
  def z3(timeout: Int = 1000) = smt("z3", "-t:" + timeout, "-in")
  def cvc4(timeout: Int = 1000) = smt("cvc4", "--tlimit=" + timeout, "--lang=smt2", "--incremental", "--increment-triggers")
  def princess(timeout: Int = 1000) = smt("princess", "+stdin", "+quiet", "-timeoutPer=" + timeout, "+incremental")

  var debug = false

  def smt(args: String*) = new process(args: _*) {
    override def define(id: Id, proc: Proc) = {
      Success
    }

    override def define(sort: Sort, obj: Obj) = {
      Success
    }
  }

  case class process(args: String*) extends Solver {
    val pb = new ProcessBuilder(args: _*)
    val pr = pb.start()
    val pid = pr.pid
    val stdout = new BufferedReader(new InputStreamReader(pr.getInputStream))
    val stdin = new PrintStream(pr.getOutputStream)

    if (debug) println(args.mkString("$ ", " ", "") + " # " + pid)

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

    def push(depth: Int) = {
      write(Printer.push(depth))
      Ack.from(read())
    }

    def pop(depth: Int) = {
      write(Printer.pop(depth))
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

    def declare(arities: List[Arity], decls: List[Datatype]) = {
      write(Printer.declare(arities, decls))
      Ack.from(read())
    }

    def define(id: Id, proc: Proc) = {
      write(Printer.define(id, proc))
      Ack.from(read())
    }

    def define(sort: Sort, obj: Obj) = {
      write(Printer.define(sort, obj))
      Ack.from(read())
    }

    def write(line: String) {
      if (debug) println(pid + " < " + line)
      stdin.println(line)
      stdin.flush()
    }

    def read() = {
      val line = stdout.readLine()
      if (debug) println(pid + " > " + line)
      line
    }
  }

  case class file(out: File) extends print {
    val stream = new PrintStream(out)
  }

  object stdout extends print {
    def stream = System.out
  }

  object stderr extends print {
    def stream = System.err
  }

  class capture extends print {
    val buffer = new ByteArrayOutputStream
    val stream = new PrintStream(buffer)
    override def toString = buffer.toString
  }

  abstract class print extends Solver {
    def stream: PrintStream

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

    def push(depth: Int) = {
      write(Printer.push(depth))
      Success
    }

    def pop(depth: Int) = {
      sat = Unknown
      write(Printer.pop(depth))
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
      write(Printer.assert(expr))
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

    def define(id: Id, proc: Proc) = {
      write(Printer.define(id, proc))
      Success
    }

    def define(sort: Sort, obj: Obj): Ack = {
      write(Printer.define(sort, obj))
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

  /**
   * Wraps around two solvers passing commands to both.
   *
   * @param a the result of this solver will be returned
   * @param b each command will be passed to this solver first. The result is ignored. Exceptions are not caught, however.
   */
  case class tee(primary: Solver, others: Solver*) extends Solver {
    def setLogic(logic: String): Ack = {
      for (solver <- others) solver.setLogic(logic)
      primary.setLogic(logic)
    }

    def setOption(args: List[String]): Ack = {
      for (solver <- others) solver.setOption(args)
      primary.setOption(args)
    }

    def reset(): Ack = {
      for (solver <- others) solver.reset()
      primary.reset()
    }

    def push(depth: Int): Ack = {
      for (solver <- others) solver.push(depth)
      primary.push(depth)
    }

    def pop(depth: Int): Ack = {
      for (solver <- others) solver.pop(depth)
      primary.pop(depth)
    }

    def exit(): Ack = {
      for (solver <- others) solver.exit()
      primary.exit()
    }

    def check(): IsSat = {
      for (solver <- others) solver.check()
      primary.check()
    }

    def assertions(): Assertions = {
      for (solver <- others) solver.assertions()
      primary.assertions()
    }

    def model(): Model = {
      for (solver <- others) solver.model()
      primary.model()
    }

    def declare(sort: Sort, arity: Int): Ack = {
      for (solver <- others) solver.declare(sort, arity)
      primary.declare(sort, arity)
    }

    def define(sort: Sort, args: List[Sort], body: Type): Ack = {
      for (solver <- others) solver.define(sort, args, body)
      primary.define(sort, args, body)
    }

    def declare(id: Id, args: List[Type], res: Type): Ack = {
      for (solver <- others) solver.declare(id, args, res)
      primary.declare(id, args, res)
    }

    def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean): Ack = {
      for (solver <- others) solver.define(id, formals, res, body, rec)
      primary.define(id, formals, res, body, rec)
    }

    def declare(arities: List[Arity], decls: List[Datatype]): Ack = {
      for (solver <- others) solver.declare(arities, decls)
      primary.declare(arities, decls)
    }

    def define(id: Id, proc: Proc): Ack = {
      for (solver <- others) solver.define(id, proc)
      primary.define(id, proc)
    }

    def define(sort: Sort, obj: Obj): Ack = {
      for (solver <- others) solver.define(sort, obj)
      primary.define(sort, obj)
    }

    def assert(expr: Expr): Ack = {
      for (solver <- others) solver.assert(expr)
      primary.assert(expr)
    }
  }
}

