package cuvee

import java.io.File
import java.io.FileInputStream

import scala.io.StdIn
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream

case class Cuvee(sink: Sink, config: Config) extends Solver {
  var states: List[State] = List(State.default)

  val (backend, solver) = sink match {
    case solver: Solver =>
      (solver, solver)
    case _ =>
      val solver = Solver.default
      val backend = Sink.tee(sink, solver)
      (backend, solver)
  }

  def top = states.head

  def setLogic(logic: String) = {
    backend.setLogic(logic)
  }

  def setOption(args: List[String]) = args match {
    case List(":produce-models", flag) =>
      config.produceModels = flag.toBoolean
      backend.setOption(args)

    case List(":print-success", flag) =>
      config.printSuccess = flag.toBoolean
      Success

    case _ =>
      backend.setOption(args)
  }

  def setInfo(attr: String, arg: Option[Any]): Ack = {
    backend.setInfo(attr, arg)
  }

  def report(res: Option[Res]): Option[Res] = res match {
    case None => None
    case Some(Success) if !config.printSuccess => None
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
    ack // ignored
  }

  def _pop() = {
    ensure(states.nonEmpty, "cannot pop from empty solver stack")
    val st :: rest = states
    states = rest
    st
  }

  def _push(st: State) {
    states = st :: states
  }

  def pop(depth: Int) = {
    depth times { _pop() }
    backend.pop(depth)
  }

  def push(depth: Int) = {
    depth times { _push(top) }
    backend.push(depth)
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

  override def check(expected: IsSat) = {
    val actual = check()
    if (config.test)
      ensure(expected == actual, "check-sat command returned unexpected result", actual, expected)
    actual
  }

  def check() = backend.scoped {
    var _asserts = top.asserts map eval

    if (config.simplify) {
      val simplify = Simplify(solver)
      _asserts = simplify(_asserts)
    }

    for (expr <- _asserts) {
      backend.assert(expr)
    }

    val res = backend.check()

    if (config.produceModels) {
      val model = backend.model()
      map(_ withModel model)
    }

    res
  }

  def assertions() = {
    Assertions(top.asserts)
  }

  def model() = {
    unwrap(top.model, "no model available")
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
    // backend.define(id, proc)
    Success
  }

  def define(sort: Sort, obj: Obj): Ack = {
    map(_ define (sort, obj))
    // backend.define(sort, obj)
    Success
  }

  /* def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean) = {
    val xs = formals map (_.id)
    val args = formals map (_.typ)
    val axiom = Forall(formals, App(id, xs) === body)
    map(_ declare (id, args, res))
    map(_ assert axiom)
    backend.declare(id, args, res)
  } */

  def verify(spec: Sort, impl: Sort, sim: Sim): Ack = {
    val verify = Verify(top)
    val phi = verify(spec, impl, sim)
    assert(!phi)
    // ensure(res forall (_ == Unsat), "incorrect refinement", spec, impl)
    // Success
  }

  def verify(id: Id): Ack = {
    val verify = Verify(top)
    val phi = verify(id)
    assert(!phi)
    // ensure(res == Unsat, "incorrect contract", id)
    // Success
  }

  def declare(arities: List[Arity], decls: List[Datatype]) = {
    map(_ declare (arities, decls))
    backend.declare(arities, decls)
  }
}

class Config {
  var simplify = false
  var test = false
  var printSuccess = false
  var produceModels = false
}

object Task {
  def apply(): Task = {
    new Task
  }

  def apply(source: Source, sink: Sink, report: Report): Task = {
    val task = new Task
    task.configure(source, sink, report)
    task
  }

  def apply(args: List[String]): Task = {
    val task = new Task
    task.configure(args)
    task
  }
}

class Task extends Runnable { /* because why not */
  var config = new Config
  var timeout = 1000
  var source: Source = Source.stdin
  var sink: Sink = Sink.stdout
  var report: Report = Report.stderr

  def configure(_source: Source, _sink: Sink, _report: Report) = {
    source = _source
    sink = _sink
    report = _report
  }

  def configure(args: List[String]): Unit = args match {
    case Nil =>

    case "-timeout" :: arg :: rest =>
      timeout = arg.toInt
      configure(rest)

    case "-test" :: rest =>
      config.test = true
      configure(rest)

    case "-simplify" :: rest =>
      config.simplify = true
      configure(rest)

    case "-no-simplify" :: rest =>
      config.simplify = false
      configure(rest)

    case "-debug-simplify" :: rest =>
      Simplify.debug = true
      configure(rest)

    case "-debug-verify" :: rest =>
      Verify.debug = true
      configure(rest)

    case "-debug-solver" :: rest =>
      Solver.debug = true
      configure(rest)

    case "-format" :: rest =>
      Printer.format = true
      configure(rest)

    case "-z3" :: rest =>
      ensure(rest.isEmpty, "-z3 must be the last argument")
      sink = Solver.z3(timeout)
      report = Report.stdout

    case "-cvc4" :: rest =>
      ensure(rest.isEmpty, "-cvc4 must be the last argument")
      sink = Solver.cvc4(timeout)
      report = Report.stdout

    case "-princess" :: rest =>
      ensure(rest.isEmpty, "-princess must be the last argument")
      sink = Solver.princess(timeout)
      report = Report.stdout

    case "--" :: args =>
      ensure(args.length >= 1, "-- needs an SMT solver as argument")
      sink = Solver.process(args: _*)
      report = Report.stdout

    case "-o" :: path :: rest =>
      ensure(rest.isEmpty, "-o <file> must be the last argument")
      val out = new File(path)
      sink = Sink.file(out)
      report = Report.stdout

    case "-o" :: _ =>
      error("-o needs an output file as argument")

    case path :: rest =>
      ensure(!path.startsWith("-"), "not an option", path)
      ensure(source == Source.stdin, "input can be given only once")
      val in = new File(path)
      source = Source.file(in)
      configure(rest)
  }

  def run() = {
    val cuvee = Cuvee(sink, config)
    source.run(cuvee, report)
  }
}

object Cuvee {
  def run(source: Source, backend: Sink, report: Report) {
    val task = Task(source, backend, report)
    task.run()
  }

  def run(args: List[String]): Unit = {
    val task = Task(args)
    task.run()
  }

  def main(args: Array[String]) {
    run(args.toList)
  }
}