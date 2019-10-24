package tacas2020

import scala.io.StdIn
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileReader

object Main {
  import Parser.script
  import Parser.whitespace

  object solver extends Solver

  var _in = new BufferedReader(new InputStreamReader(System.in))
  var _out = System.out

  def in() = {
    var line = _in.readLine()
    if(line != null) line = line.trim
    line
  }

  def out(any: Any) {
    _out.println(any)
    _out.flush()
  }

  def cmd(line: String) {
    try {
      val cmds = script parse line
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

  def main(args: Array[String]) {
    _in = new BufferedReader(new FileReader("test.smt2"))
    repl()
  }
}