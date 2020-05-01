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
      /*
       * If the sink is a solver, we don't need to preserve any relation between
       * input and output, so we can use it to check everything. Anything that is
       * done by auxiliary tools like the Simplifier is scoped.
       */
      (solver, solver)
    case _ =>
      val solver = Solver.default
      /*
       * If we use a dedicated solver for auxiliary tools like the simplifier,
       * declarations will still need to be visible to the simplifier. As the
       * second argument of the tee sink, everything is also passed to the auxiliary
       * solver, but the result is ignored. Assertions are collected by Cuvee and only checked
       * within a local scope and will not interfere with anything else that is done
       * in the auxiliary solver.
       */
      val backend = Sink.tee(sink, solver)
      (backend, solver)
  }

  def top = states.head

  for (logic <- config.logic) {
    backend.setLogic(logic)
  }

  def setLogic(logic: String) = config.logic match {
    case Some(logic) =>
      error(s"logic already set to ${logic}")
    case None =>
      config.logic = Some(logic)
      backend.setLogic(logic)
  }

  def setOption(args: List[String]) = args match {
    case List(":produce-models", flag) =>
      config.produceModels = flag.toBoolean
      backend.setOption(args)

    case List(":print-success", flag) =>
      config.printSuccess = flag.toBoolean
      Success

    case List(":declare-implied", flag) =>
      config.declareImplied = flag.toBoolean
      Success

    case _ =>
      backend.setOption(args)
  }

  def setInfo(attr: String, arg: Option[Any]): Ack = (attr, arg) match {
    case (":status", Some(arg: String)) =>
      val status = IsSat.from(arg)
      map(_ withStatus status)
      Success
    case _ =>
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
    backend.exit()
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
      case SuggestedDeclarationError(suggestion, _) if config.declareImplied =>
        _push(st)
        suggestion(this)
        // then retry
        map(action)
      case e: Throwable =>
        _push(st)
        throw e
    }
  }

  def eval(expr: Expr): Expr = {
    val env = top.env
    val old = Nil
    Eval(top).eval(expr, env, old)
  }

  def eval(formals: List[Formal], expr: Expr): Expr = {
    val env = top.env
    val old = Nil
    Eval(top).eval(expr, env bind formals, old)
  }

  def check() = backend.scoped {
    val rasserts = top.rasserts map eval

    val actual = rasserts match {
      case Not(phi) :: rest if config.prove =>
        val _asserts = rest.reverse

        for (expr <- _asserts) {
          backend.assert(expr)
        }

        // val _phi = Simplify.norm(phi)
        // val conj = And.flatten(_phi)
        // val unknown = conj filterNot solver.isTrue

        val prove = Prove(solver)
        val _phi = prove(phi, top)

        if (_phi == True) {
          Unsat
        } else {
          backend.assert(Not(_phi))

          sink.check()
        }

      case _ =>
        var _asserts = rasserts.reverse

        if (config.simplify) {
          val simplify = Simplify(solver)
          _asserts = simplify(_asserts)
        }

        for (expr <- _asserts) {
          backend.assert(expr)
        }

        // Make sure not to go through the simplifier backend
        // unless we're actually solving something.
        sink.check()
    }

    for (expected <- top.status if config.test) {
      ensure(expected == actual, "check-sat command returned unexpected result", actual, expected)
    }

    if (config.produceModels) {
      val model = backend.model()
      map(_ withModel model)
    }

    actual
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
    val _body = eval(formals, body)
    backend.define(id, formals, res, _body, rec)
  }

  def define(id: Id, proc: Proc): Ack = {
    map(_ define (id, proc))
    Success
  }

  def define(sort: Sort, obj: Obj): Ack = {
    map(_ define (sort, obj))
    Success
  }

  def verify(spec: Sort, impl: Sort, sim: Sim): Ack = {
    val A = top objects spec
    val C = top objects impl

    val (defs, phi) = Verify.refinement(A, C, sim, top, solver)
    (defs, sim) match {
      case (List(Forall(formals, Eq(App(r, args), deff))), Sim.byFun(r_, _)) if r == r_ && formals.ids == args =>
        // This is a simple R = ... definition which we can use define-fun for.
        // This is more convenient for the backend and allows us to prove more complicated examples.
        define(r, formals, Sort.bool, deff, false)
      case _ =>
        assert(defs)
    }

    config.logic match {
      case Some("HORN") =>
        val conds = Horn.split(eval(phi)) map (_.toExpr)
        assert(conds)
        Success
      case _ =>
        assert(!phi)
    }
  }

  def verify(id: Id): Ack = {
    val proc = top procdefs id
    val phi = Verify.contract(proc, Nil)
    assert(!phi)
  }

  def verify(sort: Sort): Ack = {
    val obj = top objects sort
    val phi = Verify.contract(obj)
    assert(!phi)
  }

  def declare(arities: List[Arity], decls: List[Datatype]) = {
    map(_ declare (arities, decls))
    backend.declare(arities, decls)
  }
}

class Config {
  var logic = None: Option[String]
  var prove = false
  var simplify = false
  var test = false
  var printSuccess = false
  var produceModels = false
  var declareImplied = false
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

    case "-logic" :: logic :: rest =>
      config.logic = Some(logic)
      configure(rest)

    case "-prove" :: rest =>
      config.prove = true
      configure(rest)

    case "-simplify" :: rest =>
      config.simplify = true
      configure(rest)

    case "-no-simplify" :: rest =>
      config.simplify = false
      configure(rest)

    case "-declare-implied" :: rest =>
      config.declareImplied = true
      configure(rest)

    case "-qe" :: rest =>
      Simplify.qe = true
      configure(rest)

    case "-no-qe" :: rest =>
      Simplify.qe = false
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

    case "-inferInvariants" :: rest =>
      Eval.inferInvariants = true
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
    try {
      source.run(cuvee, report)
    } finally {
      cuvee.exit();
    }
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