package cuvee

import java.io.File
import java.io.FileInputStream

import scala.io.StdIn
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream

case class Cuvee(prover: Solver[SmtCmd], backend: Solver[SmtCmd]) extends ExtSolver {
  var states: List[State] = List(State.default)

  var printSuccess = false
  var produceModels = false

  override def toString = log.mkString("\n")
  val simplify = Simplify(prover)

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
    val _expr = Eval.eval(expr, env, old, top)
    if (Cuvee.simplify) {
      simplify(_expr)
    } else {
      _expr
    }
  }

  def check() = backend.scoped {
    val _asserts = top.asserts map eval

    for (expr <- _asserts) {
      backend.assert(expr)
    }

    val res = backend.check()

    if (produceModels) {
      val model = backend.model()
      map(_ withModel model)
    }

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
    map(_ define (id, formals, res, body))
    backend.define(id, formals, res, body, rec)
  }

  def define(id: Id, proc: Proc): Ack = {
    map(_ define (id, proc))
    backend match {
      case solver: ExtSolver =>
        solver.define(id, proc)
      case _ => Success
    }
  }

  def define(sort: Sort, obj: Obj): Ack = {
    map(_ define (sort, obj))
    backend match {
      case solver: ExtSolver =>
        solver.define(sort, obj)
      case _ => Success
    }
  }

  /* def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean) = {
    val xs = formals map (_.id)
    val args = formals map (_.typ)
    val axiom = Forall(formals, App(id, xs) === body)
    map(_ declare (id, args, res))
    map(_ assert axiom)
    backend.declare(id, args, res)
  } */

  def declare(arities: List[Arity], decls: List[Datatype]) = {
    map(_ declare (arities, decls))
    backend.declare(arities, decls)
  }
}

object Cuvee {
  var simplify = true

  def run[C >: SmtCmd <: Cmd](source: Source[Cmd], prover: Solver[C], backend: Solver[C], report: Report) {
    val solver = Cuvee(prover, backend)
    source.run(solver, report)
  }

  def run[C >: SmtCmd <: Cmd](source: Source[Cmd], backend: Solver[C], report: Report) {
    run(source, backend, backend, report)
  }

  def runWithArgs[C >: SmtCmd <: Cmd](args: List[String], source: Source[Cmd], prover: Solver[C], backend: Solver[C], report: Report): Unit = args match {
    case Nil =>
      run(source, ???, backend, report)

    case "-simplify" :: rest =>
      simplify = true
      runWithArgs(rest, source, prover, backend, report)

    case "-no-simplify" :: rest =>
      simplify = false
      runWithArgs(rest, source, prover, backend, report)

    case "-debug-solver" :: rest =>
      Solver.traffic = true
      runWithArgs(rest, source, prover, backend, report)

    case "-z3" :: rest =>
      ensure(rest.isEmpty, "-z3 must be the last argument")
      run(source, Solver.z3(), report)

    case "-cvc4" :: rest =>
      ensure(rest.isEmpty, "-cvc4 must be the last argument")
      run(source, Solver.cvc4(), report)

    case "-princess" :: rest =>
      ensure(rest.isEmpty, "-princess must be the last argument")
      run(source, Solver.princess(), report)

    case "--" :: args =>
      ensure(args.length >= 1, "-- needs an SMT solver as argument")
      val _solver = Solver.process(args: _*)
      run(source, _solver, report)

    case "-o" :: path :: rest =>
      val out = new File(path)
      val _report = Report.file(out)
      runWithArgs(rest, source, prover, backend, _report)

    case path :: rest =>
      ensure(source == Source.stdin(Cmd), "input can be given only once")
      val in = new File(path)
      val _source = Source.file(in, ExtScript)
      runWithArgs(rest, _source, prover, backend, report)
  }

  def run(args: List[String]) {
    runWithArgs(args, Source.stdin(Cmd), Solver.default, Solver.stdout, Report.stdout)
  }

  def main(args: Array[String]) {
    run(args.toList)
  }
}