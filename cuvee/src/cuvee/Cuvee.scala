package cuvee

import java.io.File
import java.io.FileInputStream

import scala.io.StdIn
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream

case class Cuvee(backend: Solver, config: Config) extends Solver {
  var states: List[State] = List(State.default)

  override def toString = log.mkString("\n")

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
    ensure(!states.isEmpty, "empty solver stack")
    val st :: rest = states
    states = rest
    st
  }

  def _push(st: State) {
    states = st :: states
  }

  def pop(depth: Int) = {
    _pop()
    backend.pop(depth)
  }

  def push(depth: Int) = {
    for(i <- 0 until depth)
      _push(top)
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

  def check() = backend.scoped {
    var _asserts = top.asserts map eval

    if (config.simplify) {
      val simplify = Simplify(top.withoutAsserts)
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
    backend.define(id, proc)
  }

  def define(sort: Sort, obj: Obj): Ack = {
    map(_ define (sort, obj))
    backend.define(sort, obj)
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

class Config {
  var simplify = false
  var printSuccess = false
  var produceModels = false
}

class CuveeBuilder {
  var settings = new Config
  var timeout = 1000
  var source: Source = Source.stdin
  var solver: Solver = Solver.stdout
  var report: Report = Report.stderr

  def configure(args: List[String]): CuveeBuilder = args match {
    case Nil =>
      this

    case "-timeout" :: arg :: rest =>
      timeout = arg.toInt
      configure(rest)

    case "-simplify" :: rest =>
      settings.simplify = true
      configure(rest)

    case "-no-simplify" :: rest =>
      settings.simplify = false
      configure(rest)

    case "-debug-simplify" :: rest =>
      Simplify.debug = true
      configure(rest)

    case "-debug-solver" :: rest =>
      Solver.debug = true
      configure(rest)

    case "-z3" :: rest =>
      ensure(rest.isEmpty, "-z3 must be the last argument")
      solver = Solver.z3(timeout)
      this

    case "-cvc4" :: rest =>
      ensure(rest.isEmpty, "-cvc4 must be the last argument")
      solver = Solver.cvc4(timeout)
      this

    case "-princess" :: rest =>
      ensure(rest.isEmpty, "-princess must be the last argument")
      solver = Solver.princess(timeout)
      this

    case "--" :: args =>
      ensure(args.length >= 1, "-- needs an SMT solver as argument")
      solver = Solver.process(args: _*)
      this

    case "-o" :: path :: rest =>
      ensure(rest.isEmpty, "-o <file> must be the last argument")
      val out = new File(path)
      solver = Solver.file(out)
      this

    case "-o" :: _ =>
      error("-o needs an output file as argument")

    case path :: rest =>
      ensure(!path.startsWith("-"), "not an option", path)
      ensure(source == Source.stdin, "input can be given only once")
      val in = new File(path)
      source = Source.file(in)
      configure(rest)
  }

  def build: Cuvee = Cuvee(solver, settings)
}

object Cuvee {
  def run(source: Source, backend: Solver, report: Report) {
    val solver = Cuvee(backend, new Config)
    source.run(solver, report)
  }

  def run(args: List[String]): Unit = {
    val builder = new CuveeBuilder
    builder.configure(args)
    builder.source.run(builder.build, builder.report)
  }

  def main(args: Array[String]) {
    run(args.toList)
  }
}