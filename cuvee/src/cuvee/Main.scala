package cuvee

import java.io.File
import java.io.FileInputStream

import scala.io.StdIn
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream

object Main {
  case class eval(backend: Solver) extends Solver {
    def setLogic(logic: String) = backend.setLogic(logic)
    def setOption(args: List[String]) = backend.setOption(args)

    def reset() = backend.reset()
    def push() = backend.push()
    def pop() = backend.pop()
    def exit() = backend.exit()

    def check() = backend.check()
    def assertions() = backend.assertions()
    def model() = backend.model()

    def declare(sort: Sort, arity: Int) = backend.declare(sort, arity)
    def define(sort: Sort, args: List[Sort], body: Type) = backend.define(sort, args, body)

    def declare(id: Id, args: List[Type], res: Type) = backend.declare(id, args, res)
    def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean) = backend.define(id, formals, res, body, rec)

    def assert(expr: Expr) = backend.assert(expr)
  }

  def run(source: Source, backend: Solver, report: Report) {
    val solver = eval(backend)
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
  }
}