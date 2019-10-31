package cuvee

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.io.File
import java.io.FileInputStream

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
}

object Source {
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
    case GetAssertions => Some(Res.empty)
    case GetModel => Some(Res.empty)
    case CheckSat => Some(Unknown)
    case Reset => Some(Success)
    case _: SetLogic => Some(Success)
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