package cuvee

import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.File

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

      case CheckSat(_) =>
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
  case class TeeSolver(a: Solver, b: Solver) extends Solver {
    override def setLogic(logic: String): Ack = {
      b.setLogic(logic)
      a.setLogic(logic)
    }

    override def setOption(args: List[String]): Ack = {
      b.setOption(args)
      a.setOption(args)
    }

    override def reset(): Ack = {
      b.reset()
      a.reset()
    }

    override def push(depth: Int): Ack = {
      b.push(depth)
      a.push(depth)
    }

    override def pop(depth: Int): Ack = {
      b.pop(depth)
      a.pop(depth)
    }

    override def exit(): Ack = {
      b.exit()
      a.exit()
    }

    override def check(): IsSat = {
      b.check()
      a.check()
    }

    override def assertions(): Assertions = {
      b.assertions()
      a.assertions()
    }

    override def model(): Model = {
      b.model()
      a.model()
    }

    override def declare(sort: Sort, arity: Int): Ack = {
      b.declare(sort, arity)
      a.declare(sort, arity)
    }

    override def define(sort: Sort, args: List[Sort], body: Type): Ack = {
      b.define(sort, args, body)
      a.define(sort, args, body)
    }

    override def declare(id: Id, args: List[Type], res: Type): Ack = {
      b.declare(id, args, res)
      a.declare(id, args, res)
    }

    override def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean): Ack = {
      b.define(id, formals, res, body, rec)
      a.define(id, formals, res, body, rec)
    }

    override def declare(arities: List[Arity], decls: List[Datatype]): Ack = {
      b.declare(arities, decls)
      a.declare(arities, decls)
    }

    override def define(id: Id, proc: Proc): Ack = {
      b.define(id, proc)
      a.define(id, proc)
    }

    override def define(sort: Sort, obj: Obj): Ack = {
      b.define(sort, obj)
      a.define(sort, obj)
    }

    override def assert(expr: Expr): Ack = {
      b.assert(expr)
      a.assert(expr)
    }
  }
}

