package cuvee

import java.io.File
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import java.io.ByteArrayOutputStream

trait Source {
  def run(solver: Solver, report: Report)
}

trait Report extends (Res => Unit) {
  def apply(res: Res)
}

trait Sink {
  def scoped[A](f: => A) = {
    push(1)
    try { f }
    finally { pop(1) }
  }

  def setLogic(logic: String): Ack
  def setOption(args: List[String]): Ack
  def setInfo(attr: String, arg: Option[Any]): Ack

  def reset(): Ack
  def push(depth: Int): Ack
  def pop(depth: Int): Ack
  def exit(): Ack

  def check(): IsSat

  def assertions(): Assertions
  def model(): Model

  def declare(sort: Sort, arity: Int): Ack
  def define(sort: Sort, args: List[Sort], body: Type): Ack

  def declare(formal: Formal): Ack = declare(formal.id, formal.typ)
  def declare(id: Id, res: Type): Ack = declare(id, Nil, res)
  def declare(id: Id, args: List[Type], res: Type): Ack
  def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean): Ack

  def verify(id: Id): Ack
  def verify(spec: Sort, impl: Sort, sim: Sim): Ack

  def declare(arities: List[Arity], decls: List[Datatype]): Ack

  def define(id: Id, proc: Proc): Ack
  def define(sort: Sort, obj: Obj): Ack

  def assert(expr: Expr): Ack

  def assert(exprs: List[Expr]) {
    for (expr <- exprs)
      assert(expr)
  }

  def setOption(args: String*): Ack = {
    setOption(args.toList)
  }

  def exec(line: String): Option[Res] = {
    exec(Cmd.from(line))
  }

  def exec(cmd: Cmd): Option[Res] = {
    cmd match {
      case SetLogic(logic) =>
        Some(setLogic(logic))
      case SetOption(args) =>
        Some(setOption(args))
      case SetInfo(attr, arg) =>
        Some(setInfo(attr, arg))

      case Reset =>
        reset(); None
      case Push(depth) =>
        push(depth); None
      case Pop(depth) =>
        pop(depth); None
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
      case DeclareDatatypes(arity, decls) =>
        Some(declare(arity, decls))

      case Assert(expr) =>
        Some(assert(expr))

      case DefineProc(id, proc) =>
        Some(define(id, proc))
      case DefineClass(sort, obj) =>
        Some(define(sort, obj))

      case VerifyProc(id) =>
        Some(verify(id))
      case VerifyRefinement(spec, impl, sim) =>
        Some(verify(spec, impl, sim))

      /* case _ =>
        Some(Error("not supported")) */
    }
  }
}

object Source {
  def safe(cmd: Cmd, solver: Solver, report: Report) {
    try {
      solver.exec(cmd) match {
        case None =>
        case Some(res) =>
          report(res)
      }
    } catch {
      // Note: Report should only catch responses from the underlying solver.
      //       Bugs/errors in Cuvee should be caught elsewhere (e.g. top-level)!
      case e: Error =>
        report(e)
      /* case t: Throwable =>
        t.printStackTrace
        val e = Error(t.toString)
        report(e) */
    }
  }

  case object stdin extends Source {
    val reader = new BufferedReader(new InputStreamReader(System.in))

    def readLine() = {
      reader.readLine()
    }

    def run(solver: Solver, report: Report) {
      var line: String = null
      do {
        line = readLine()
        if (line != null) {
          val cmd = Cmd.from(line)
          safe(cmd, solver, report)
        }
      } while (line != null)
    }
  }

  object file {
    def apply(path: String) = {
      new file(new File(path))
    }
  }

  case class file(in: File) extends Source {
    def run(solver: Solver, report: Report) {
      val cmds = Script.from(in)
      for (cmd <- cmds) {
        safe(cmd, solver, report)
      }
    }
  }
}

object Report {
  object none extends Report {
    def apply(res: Res) {
    }
  }

  object stdout extends Report {
    def apply(res: Res) {
      System.out.println(res)
      System.out.flush()
    }
  }

  object stderr extends Report {
    def apply(res: Res) {
      System.err.println(res)
      System.err.flush()
    }
  }

  case class file(out: File) extends Report {
    val stream = new PrintStream(out)

    def apply(res: Res) {
      stream.println(res)
      stream.flush()
    }
  }
}

object Sink {
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

  abstract class print extends Sink {
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

    def setInfo(attr: String, arg: Option[Any]) = {
      write(Printer.setInfo(attr, arg))
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

    def verify(id: Id): Ack = {
      write(Printer.verify(id))
      Success
    }

    def verify(spec: Sort, impl: Sort, sim: Sim) = {
      write(Printer.verify(spec, impl, sim))
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
  case class tee(primary: Sink, others: Sink*) extends Sink {
    def setLogic(logic: String): Ack = {
      for (solver <- others) solver.setLogic(logic)
      primary.setLogic(logic)
    }

    def setOption(args: List[String]): Ack = {
      for (solver <- others) solver.setOption(args)
      primary.setOption(args)
    }

    def setInfo(attr: String, arg: Option[Any]): Ack = {
      for (solver <- others) solver.setInfo(attr, arg)
      primary.setInfo(attr, arg)
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

    def verify(id: Id): Ack = {
      for (solver <- others) solver.verify(id)
      primary.verify(id)
    }

    def verify(spec: Sort, impl: Sort, sim: Sim): Ack = {
      for (solver <- others) solver.verify(spec, impl, sim)
      primary.verify(spec, impl, sim)
    }

    def assert(expr: Expr): Ack = {
      for (solver <- others) solver.assert(expr)
      primary.assert(expr)
    }
  }
}