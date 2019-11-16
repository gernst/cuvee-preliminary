package cuvee

import java.io.File
import java.io.FileInputStream

import scala.io.StdIn
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream

case class Simplify(backend: Solver) extends Solver {
  var rlog: List[Cmd] = Nil
  var states: List[State] = List(State.default)

  override def toString = {
    log.mkString("\n")
  }

  def log = rlog.reverse
  def top = states.head

  def setLogic(logic: String) = backend.setLogic(logic)
  def setOption(args: List[String]) = backend.setOption(args)

  override def exec(cmd: Cmd): Option[Res] = {
    val res = super.exec(cmd)
    res match {
      case None => None
      case Some(_: Ack) => None
      case _ => res
    }
  }

  def reset() = {
    states = List(State.default)
    backend.reset()
  }

  def exit() = {
    val ack = backend.exit()
    System.exit(0)
    ack
  }

  def _pop() = {
    ensure(!states.isEmpty, "empty stack")
    val st :: rest = states
    states = rest
    st
  }

  def _push(st: State) {
    states = st :: states
  }

  def pop() = {
    _pop()
    backend.pop()
  }

  def push() = {
    _push(top)
    backend.push()
  }

  def map(action: State => State) {
    val st = _pop()
    try {
      _push(action(st))
    } catch {
      case e: Throwable =>
        _push(st)
        throw e
    }
  }

  def simplify(expr: Expr): Expr = {
    val env = top.env
    val old = Nil
    Eval.eval(expr, env, old, top)
  }

  def check() = {
    backend.push()
    for (expr <- top.asserts) {
      val _expr = simplify(expr)
      backend.assert(_expr)
    }
    val res = backend.check()
    backend.pop()
    res
  }

  def assertions() = {
    Assertions(top.asserts)
  }

  def model() = {
    backend.model()
  }

  def assert(expr: Expr) = {
    map(_ assert expr)
    Success
  }

  def declare(sort: Sort, arity: Int) = {
    map(_ declare (sort, arity))
    backend.declare(sort, arity)
  }

  def define(sort: Sort, args: List[Sort], body: Type) = {
    map(_ define (sort, args, body))
    backend.define(sort, args, body)
  }

  def declare(id: Id, args: List[Type], res: Type) = {
    map(_ declare (id, args, res))
    backend.declare(id, args, res)
  }

  def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean) = {
    val xs = formals map (_.id)
    val args = formals map (_.typ)
    val axiom = Forall(formals, App(id, xs) === body)
    map(_ declare (id, args, res))
    map(_ assert axiom)
    backend.declare(id, args, res)
  }
}

object Cuvee {
  def run(source: Source, backend: Solver, report: Report) {
    val solver = Simplify(backend)
    source.run(solver, report)
  }

  def run(args: List[String], source: Source, solver: Solver, report: Report): Unit = args match {
    case Nil =>
      run(source, solver, report)

    case "-z3" :: rest =>
      run("--" :: "z3" :: "-in" :: rest, source, solver, report)

    case "-cvc4" :: rest =>
      run("--" :: "cvc4" :: "--lang" :: "smt2" :: rest, source, solver, report)

    case "--" :: args =>
      ensure(args.length >= 1, "-- needs an SMT solver as argument")
      val _solver = Solver.process(args: _*)
      run(source, _solver, report)

    case "-o" :: path :: rest =>
      val out = new File(path)
      val _report = Report.file(out)
      run(rest, source, solver, _report)

    case path :: rest =>
      ensure(source == Source.stdin, "only a single input file is supported")
      val in = new File(path)
      val _source = Source.file(in)
      run(rest, _source, solver, report)
  }

  def run(args: List[String]) {
    run(args.toList, Source.stdin, Solver.stdout, Report.stdout)
  }

  def main(args: Array[String]) {
    // run(args.toList)
    run(List("examples/gcd.smt2", "-z3"))
  }
}