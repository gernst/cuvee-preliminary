package cuvee

import java.io.File
import java.io.FileInputStream

import scala.io.StdIn
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream

case class Cuvee(backend: Solver) extends Solver {
  var states: List[State] = List(State.default)

  var printSuccess = false
  var produceModels = false

  override def toString = Printer.solver(this)

  def top = states.head

  def setLogic(logic: String) = {
    backend.setLogic(logic)
  }

  def setOption(args: List[String]) = args match {
    case List(":produce-models", flag) =>
      produceModels = flag.toBoolean
      backend.setOption(args)

    case List(":print-success", flag) =>
      printSuccess = flag.toBoolean
      Success

    case _ =>
      backend.setOption(args)
  }

  def report(res: Option[Res]): Option[Res] = res match {
    case None => None
    case Some(Success) if !printSuccess => None
    case _ => res
  }

  override def exec(cmd: Cmd): Option[Res] = {
    report(super.exec(cmd))
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
      _push(action(st.clearModel))
    } catch {
      case e: Throwable =>
        _push(st)
        throw e
    }
  }

  def eval(expr: Expr): Expr = {
    val env = top.env
    val old = Nil
    Eval.eval(expr, env, old, top)
  }

  def check() = {
    backend.push()
    val _asserts = top.asserts map eval
    val __asserts = Simplify.simplify(_asserts)

    for (expr <- __asserts) {
      // val _expr = eval(expr)
      backend.assert(expr)
    }

    val res = backend.check()

    if (produceModels) {
      val model = backend.model()
      map(_ withModel model)
    }

    backend.pop()
    res
  }

  def assertions() = {
    Assertions(top.asserts)
  }

  def model() = {
    top.model match {
      case None =>
        throw Error("no model available")
      case Some(model) =>
        model
    }
  }

  def assert(expr: Expr) = {
    map(_ assert expr)
    Success
  }

  def declare(sort: Sort, arity: Int) = {
    map(_ declare (sort, arity))
    backend.declare(sort, arity)
    Simplify.backend.declare(sort, arity)
  }

  def define(sort: Sort, args: List[Sort], body: Type) = {
    map(_ define (sort, args, body))
    backend.define(sort, args, body)
  }

  def declare(id: Id, args: List[Type], res: Type) = {
    map(_ declare (id, args, res))
    backend.declare(id, args, res)
    Simplify.backend.declare(id, args, res)
  }

  def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean) = {
    val xs = formals map (_.id)
    val args = formals map (_.typ)
    val axiom = Forall(formals, App(id, xs) === body)
    map(_ declare (id, args, res))
    map(_ assert axiom)
    backend.declare(id, args, res)
    Simplify.backend.declare(id, args, res)
  }
}

object Cuvee {
  def run(source: Source, backend: Solver, report: Report) {
    val solver = Cuvee(backend)
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
    run(args.toList)
    // val solver = Solver.stdout
    // val source = Source.file(new File("examples/gcd.smt2"))
    // val report = Report.stdout
    // run(source, solver, report)
  }
}
