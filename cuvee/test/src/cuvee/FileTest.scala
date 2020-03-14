package cuvee

import java.io._
import java.nio.file.Files

import cuvee.test.TestSuite

object FileTest extends TestSuite {
  val TestHeaderPrefix = ";! Cuvee "
  // File filter for debugging and unfinished, but committed tests (should usually be: Nil)
  val fileMask: List[String] = Nil

  Files.walk(new File("examples/tests").toPath)
    .filter(Files.isRegularFile(_))
    .filter(!_.getFileName.toString.endsWith(".out.smt2"))
    .filter(path => fileMask forall (mask => !(path.toString contains mask)))
    .forEach(path => test(path.toString) {
      run(path.toString)
    })

  def run(fileName: String) = {
    Expr._index = 0 // reset because we have reference output
    assert(fileName endsWith ".smt2", "file name must end with .smt2")
    val capture = new Sink.capture
    val task = configureCuvee(fileName)

    task.config.printSuccess = false
    task.config.test = true
    task.sink = Sink.tee(task.sink, capture)
    task.source = Source.file(fileName)
    task.report = result => result match {
      case e: Error => throw e
      case _ => // whatever
    }

    task.run()

    // runAndCheckResults(fileName, cuvee)
    checkVsReferenceOutput(fileName, capture.toString)
  }

  private def configureCuvee(fileName: String) = {
    val reader = new BufferedReader(new FileReader(fileName))
    try {
      val header = reader.readLine()
      if (header startsWith TestHeaderPrefix) {
        val spec = header.drop(TestHeaderPrefix.length).trim.split(' ')
        val args = spec.toList
        Task(args)
      } else {
        Task()
      }
    } finally {
      reader.close()
    }
  }

  /* private def runAndCheckResults(task: Task) = {
    val cmds = VerifyTest.runUnwrappingErrors(Source.fromFile(fileName))
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
  } */

  private def checkVsReferenceOutput(fileName: String, given: String) = {
    val outFile = new File(fileName.substring(0, fileName.length - 4) + "out.smt2")
    if (outFile.exists()) {
      val expected = VerifyTest.runUnwrappingErrors(Script.from(outFile))
      val actual = Script.from(given)
      assertEquals(actual, expected)
    }
  }
}
