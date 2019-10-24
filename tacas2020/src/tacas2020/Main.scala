package tacas2020

import java.io.File
import java.io.FileInputStream

import scala.io.StdIn

object Main {
  import Parser.script
  import Parser.whitespace

  object solver extends Solver

  def in() = {
    var line = StdIn.readLine()
    if (line != null) line = line.trim
    line
  }

  def out(any: Any) {
    Console.println(any)
    Console.flush()
  }

  def cmd(input: String) {
    try {
      val cmds = script parse input
      val msgs = solver.exec(cmds)
      for (msg <- msgs)
        out(msg)
    } catch {
      case e: Error =>
        out(e)
      case t: Throwable =>
        out("(error \"" + t + "\")")
    }
  }

  def repl() {
    while (true) {
      val line = in()
      line match {
        case null | "" => solver.exit()
        case _ => cmd(line)
      }
    }
  }

  def read(in: File) {
    val length = in.length
    val buf = new Array[Byte](length.toInt)
    val stream = new FileInputStream(in)
    val read = stream.read(buf)
    ensure(read == length, "short read", in.getPath)
    stream.close()
    val content = new String(buf, "UTF-8")
    cmd(content)
  }

  def main(args: Array[String]) {
    // repl()
    read(new File("add.smt2"))
    out(solver.top)
  }
}