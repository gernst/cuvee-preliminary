package tacas2020

import java.io.File
import java.io.FileInputStream

import scala.io.StdIn
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream

object Main {
  import Parser.script
  import Parser.whitespace

  var solver: Solver = null

  def in() = {
    var line = StdIn.readLine()
    if (line != null) line = line.trim
    line
  }

  var _out = System.out

  def out(any: Any) {
    _out.println(any)
    _out.flush()
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
    solver = new Solver
    while (true) {
      val line = in()
      line match {
        case null | "" => solver.exit()
        case _ => cmd(line)
      }
    }
  }

  def read(path: String) {
    solver = new Solver
    val file = new File(path)
    val length = file.length
    val buf = new Array[Byte](length.toInt)
    val stream = new FileInputStream(file)
    val read = stream.read(buf)
    ensure(read == length, "short read", path)
    stream.close()
    val content = new String(buf, "UTF-8")
    cmd(content)
    out(solver.top)
    solver = null
  }

  def drain(in: InputStream) {
    val reader = new BufferedReader(new InputStreamReader(in))
    var line: String = null
    do {
      line = reader.readLine()
      if (line != null)
        out(line)
    } while (line != null)
  }

  def run(args: List[String], files: List[String]): Unit = args match {
    case Nil if files.isEmpty =>
      repl()

    case Nil =>
      for (file <- files.reverse)
        read(file)

    case "--" :: args =>
      ensure(args.length >= 1, "-- needs an SMT solver as argument")

      for (file <- files.reverse) {
        val pb = new ProcessBuilder(args: _*)
        val pr = pb.start()
        val stdout = pr.getInputStream
        val stderr = pr.getErrorStream
        val stdin = new PrintStream(pr.getOutputStream)
        _out = stdin
        read(file)
        out("(exit)")
        _out = System.out
        drain(stdout)
        drain(stderr)
        stdin.close()
      }

    case "-o" :: dest :: Nil =>
      ensure(files.length == 1, "-o can only be used with a single input file", files)
      _out = new PrintStream(new File(dest))

      for (file <- files.reverse)
        read(file)

    case file :: rest =>
      run(rest, file :: files)
  }

  def main(args: Array[String]) {
    run(args.toList, Nil)
  }
}