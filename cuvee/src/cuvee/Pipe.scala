package cuvee

import java.io.File
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream

trait Source extends Runnable {
}

trait Report extends (Res => Unit) {
}

object Source {
  def safe(cmd: Cmd, solver: Solver, out: Report) {
    try {
      solver.exec(cmd) match {
        case None =>
        case Some(res) =>
          out(res)
      }
    } catch {
      case e: Error =>
        out(e)
      case t: Throwable =>
        t.printStackTrace
        val e = Error(t.toString)
        out(e)
    }
  }

  case class stdin(out: Report, solver: Solver) extends Source {
    val reader = new BufferedReader(new InputStreamReader(System.in))

    def readLine() = {
      reader.readLine()
    }

    def run() {
      var line: String = null
      do {
        line = readLine()
        if (line != null) {
          val cmd = Cmd.from(line)
          safe(cmd, solver, out)
        }
      } while (line != null)
    }
  }

  case class file(in: File, out: Report, solver: Solver) extends Source {
    def read() = {
      val length = in.length
      val buf = new Array[Byte](length.toInt)
      val stream = new FileInputStream(in)
      val read = stream.read(buf)
      ensure(read == length, "short read", in)
      stream.close()
      new String(buf, "UTF-8")
    }

    def run() {
      val input = read()
      val cmds = Script.from(input)

      for (cmd <- cmds) {
        safe(cmd, solver, out)
      }
    }
  }
}

object Report {
  object stdout extends Report {
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