package cuvee

import java.io._
import java.nio.file.Files

import cuvee.Solver.TeeSolver
import cuvee.Source.file
import cuvee.test.TestSuite

object FileTest extends TestSuite {
  Files.walk(new File("examples/tests").toPath)
    .filter(Files.isRegularFile(_))
    .filter(!_.getFileName.toString.endsWith(".out.smt2"))
    .forEach(path => test(path.toString) {
      run(path.toString)
    })

  def run(fileName: String) = {
    Expr._index = 0 // reset because we have reference output
    assert(fileName endsWith ".smt2", "file name must end with .smt2")
    val buffer = new ByteArrayOutputStream
    val cuvee: Cuvee = buildCuvee(fileName, buffer)
    runAndCheckResults(fileName, cuvee)
    checkVsReferenceOutput(fileName, buffer)
  }

  private def buildCuvee(fileName: String, buffer: ByteArrayOutputStream) = {
    val builder = configureCuvee(fileName)
    builder.settings.printSuccess = true
    val ps = new PrintStream(buffer)
    Cuvee(TeeSolver(builder.solver, Solver.print(ps)), builder.settings)
  }

  private def configureCuvee(fileName: String) = {
    val reader = new BufferedReader(new FileReader(fileName))
    try {
      val header = reader.readLine()
      if (header.startsWith(";")) {
        val args = header.substring(1).trim.split(' ').toList
        new CuveeBuilder().configure(args)
      } else {
        new CuveeBuilder
      }
    } finally {
      reader.close()
    }
  }

  private def runAndCheckResults(fileName: String, cuvee: Cuvee) = {
    val cmds = VerifyTest.runUnwrappingErrors(file(new File(fileName)).cmds)
    for (cmd <- cmds) {
      cuvee.exec(cmd) match {
        case Some(res) => cmd match {
          case CheckSat(Some(expected)) => assertEquals(res, expected)
          case _ => res match {
            case _: IsSat =>
              assertEquals(Unsat, res)
            case _: Ack =>
              assertEquals(Success, res)
          }
        }
      }
    }
  }

  private def checkVsReferenceOutput(fileName: String, buffer: ByteArrayOutputStream) = {
    val outFile = new File(fileName.substring(0, fileName.length - 4) + "out.smt2")
    if (outFile.exists()) {
      val reference = VerifyTest.runUnwrappingErrors(file(outFile).cmds)
      val actual = Script.from(buffer.toString)
      assertEquals(actual, reference)
    }
  }
}
