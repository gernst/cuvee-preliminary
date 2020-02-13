package cuvee

import java.io.File
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream

trait Source {
  def run(solver: Solver, report: Report)
}

trait Report extends (Res => Unit) {
  def apply(res: Res)
}

object Source {
  def safe(cmd: Cmd, solver: Solver, report: Report) {
    try {
      solver.exec(cmd) match {
        case None =>
        case Some(res) =>
          report(res)
      }
    } catch {
      case e: Error =>
        report(e)
      case t: Throwable =>
        t.printStackTrace
        val e = Error(t.toString)
        report(e)
    }
  }

  case object stdin extends Source {
    val reader = new BufferedReader(new InputStreamReader(System.in))

    def readLine() = {
      reader.readLine()
    }

    def run(solver: Solver, report: Report) {
      var line: String = null
      do {
        line = readLine()
        if (line != null) {
          val cmd = Cmd.from(line)
          safe(cmd, solver, report)
        }
      } while (line != null)
    }
  }

  case class file(in: File) extends Source {
    def read() = {
      val length = in.length
      val buf = new Array[Byte](length.toInt)
      val stream = new FileInputStream(in)
      val read = stream.read(buf)
      ensure(read == length, "short read", in)
      stream.close()
      new String(buf, "UTF-8")
    }

    def cmds = {
      Script.from(read())
    }

    def run(solver: Solver, report: Report) {
      for (cmd <- cmds) {
        safe(cmd, solver, report)
      }
    }
  }
}

object Report {
  case object stdout extends Report {
    def apply(res: Res) {
      System.out.println(res)
      System.out.flush()
    }
  }

  case class file(out: File) extends Report {
    val stream = new PrintStream(out)

    def apply(res: Res) {
      stream.println(res)
      stream.flush()
    }
  }
}