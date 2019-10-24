package tacas2020

object Main {
  import Parser.script
  import Parser.whitespace

  object solver extends Solver

  def cmd(line: String) {
    try {
      val cmds = script parse line
      solver.exec(cmds)
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
      if (line == null) solver.exit()
      else cmd(line)
    }
  }

  def main(args: Array[String]) {
    repl()
  }
}