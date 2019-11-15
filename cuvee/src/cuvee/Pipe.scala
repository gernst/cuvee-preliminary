package cuvee

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.io.File
import java.io.FileInputStream

import scala.annotation.tailrec

case class Pipe(source: Source, sink: Sink) extends Runnable {
  def run() {
    while (true) {
      val cmd = source.recv()
      if (cmd == null) return
      sink.send(cmd)
    }
  }
}

object Pipe {
  case class log(to: Sink) extends Sink {
    var rcmds: List[Cmd] = Nil
    var rress: List[Res] = Nil

    def cmds = rcmds.reverse
    def ress = rress.reverse

    def send(cmd: Cmd) = {
      rcmds = cmd :: rcmds
      to.send(cmd)
    }

    def res(): Res = {
      val res = to.res()
      rress = res :: rress
      res
    }
  }

  def state(to: (() => State) => Sink): Sink = {
    object scope {
      def top(): State = st.top
      val st = state(to(top))
    }
    scope.st
  }

  case class state(to: Sink) extends Sink {
    var states: List[State] = _
    reset()

    def top = states.head

    def reset() {
      states = List(State.default)
    }

    def pop() = {
      ensure(!states.isEmpty, "empty stack")
      val st :: rest = states
      states = rest
      st
    }

    def push(st: State) {
      states = st :: states
    }

    def map(action: State => State) {
      val st0 = pop()
      try {
        push(action(st0))
      } catch {
        case e: Throwable =>
          push(st0)
          throw e
      }
    }

    def send(cmd: Cmd) = cmd match {
      case Reset =>
        reset()
        to.send(cmd)

      case Push =>
        push(top)
        to.send(cmd)

      case Pop =>
        pop()
        to.send(cmd)

      /*  case GetAssertions =>
        val asserts = top.asserts
        for (assert <- asserts)
          yield "(assert " + assert + ")" */

      case DeclareSort(sort, arity) =>
        map(_ declare (sort, arity))
        to.send(cmd)

      case DefineSort(sort, args, body) =>
        map(_ define (sort, args, body))
        to.send(cmd)

      case DeclareFun(id, args, res) =>
        map(_ declare (id, args, res))
        to.send(cmd)

      case DefineFun(id, args, res, body) =>
        map(_ define (id, args, res, body))
        to.send(cmd)

      case DefineFunRec(id, args, res, body) =>
        map(_ define (id, args, res, body))
        to.send(cmd)

      case _ =>
        to.send(cmd)
    }

    def res(): Res = {
      to.res()
    }
  }
}

object Transform {
  case class eval(state: () => State, to: Sink) extends Sink {
    def eval(expr: Expr): Expr = {
      eval(Nil, expr)
    }

    def eval(formals: List[Formal], expr: Expr): Expr = {
      val st = state()
      Eval.eval(expr, st.env bind formals, List.empty, st)
    }

    def send(cmd: Cmd) = cmd match {
      case Assert(expr) =>
        to.send(Assert(eval(expr)))
      case DefineFun(id, formals, res, body) =>
        to.send(DefineFun(id, formals, res, eval(formals, body)))
      case DefineFunRec(id, formals, res, body) =>
        to.send(DefineFunRec(id, formals, res, eval(formals, body)))
      case _ =>
        to.send(cmd)
    }

    def res(): Res = {
      to.res()
    }
  }

}

trait Inbox {
  val _in: BlockingQueue[Cmd] = new LinkedBlockingQueue[Cmd]
  def recv(): Cmd = _in.take()
  def send(cmd: Cmd) = _in.put(cmd)
}

trait Outbox {
  val _out: BlockingQueue[Res] = new LinkedBlockingQueue[Res]
  def ack(res: Res) = _out.put(res)
  def res(): Res = _out.take()
}

trait Source {
  def recv(): Cmd
  def ack(res: Res)
  def |(that: Sink) = Pipe(this, that)
}

object Source {
  def file(in: File): file = file(in, println)

  case class file(in: File, out: Res => Unit) extends Source {
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(in)))

    def recv(): Cmd = {
      val line = reader.readLine()
      if (line == null) null
      else Cmd.from(line)
    }

    def ack(res: Res) = {
      out(res)
    }
  }
}

trait Sink {
  def send(cmd: Cmd)
  def res(): Res
}

object Sink {
  def z3(timeout: Int = 1000) = process("z3", "-t:" + timeout, "-in")
  def cvc4(timeout: Int = 1000) = process("cvc4", "--tlimit=" + timeout, "--lang=smt2", "--increment-triggers")

  case class process(args: String*) extends Sink {
    val pb = new ProcessBuilder(args: _*)
    val pr = pb.start()
    val stdout = new BufferedReader(new InputStreamReader(pr.getInputStream))
    val stdin = new PrintStream(pr.getOutputStream)

    stdin.println(sexpr("set-option", ":print-success", true))
    stdin.flush()

    def send(cmd: Cmd) = {
      stdin.println(cmd)
      stdin.flush()
    }

    def res(): Res = {
      val line = stdout.readLine()
      if (line == null) null
      else Res.from(line)
    }
  }

  def dummy(in: Cmd): Option[Res] = in match {
    case Push | Pop | Exit => None
    case GetAssertions => Some(Assertions(Nil))
    case GetModel => Some(Model(Nil))
    case CheckSat => Some(Unknown)
    case Reset => Some(Success)
    case _: SetLogic => Some(Success)
    case _: SetOption => Some(Success)
    case _: DeclareSort => Some(Success)
    case _: DeclareFun => Some(Success)
    case _: DefineSort => Some(Success)
    case _: DefineFun => Some(Success)
    case _: DefineFunRec => Some(Success)
    case _: Assert => Some(Success)
  }

  def file(out: File): file = file(dummy, out)

  case class file(in: Cmd => Option[Res], out: File) extends Sink with Outbox {
    val stream = new PrintStream(out)

    def send(cmd: Cmd) = {
      stream.println(cmd)
      stream.flush()
      for (res <- in(cmd))
        ack(res)
    }
  }
}