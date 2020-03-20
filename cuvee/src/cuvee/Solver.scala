package cuvee

import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.File
import java.io.ByteArrayOutputStream

trait Solver extends Sink {
  def check(phi: Expr): IsSat = scoped {
    assert(phi)
    check()
  }

  def isTrue(phi: Expr) = {
    (phi == True) || isUnsat(!phi)
  }

  def isFalse(phi: Expr) = {
    (phi == False) || isUnsat(phi)
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

  def binding[A](formals: List[Formal])(thunk: => A) = scoped {
    bind(formals)
    thunk
  }

  def asserting[A](expr: Expr)(thunk: => A) = scoped {
    assert(expr)
    thunk
  }
}

object Solver {
  def default = z3()
  def z3(timeout: Int = 1000) = process("z3", "-t:" + timeout, "-in")
  def cvc4(timeout: Int = 1000) = process("cvc4", "--tlimit=" + timeout, "--lang=smt2", "--incremental", "--increment-triggers")
  def princess(timeout: Int = 1000) = process("princess", "+stdin", "+quiet", "-timeoutPer=" + timeout, "+incremental")

  var debug = false

  case class process(args: String*) extends Solver {
    val pb = new ProcessBuilder(args: _*)
    val pr = pb.start()
    def pid = pr.pid
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

    def setInfo(attr: String, arg: Option[Any]): Ack = {
      write(Printer.setInfo(attr, arg))
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

    def verify(id: Id): Ack = {
      write(Printer.verify(id))
      Ack.from(read())
    }

    def verify(spec: Sort, impl: Sort, sim: Sim) = {
      write(Printer.verify(spec, impl, sim))
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
}

