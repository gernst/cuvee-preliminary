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
    Printer.format = false // reset because this can be set by the .smt2 file and affects output
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

  private def checkVsReferenceOutput(fileName: String, given: String) = {
    val outFile = new File(fileName.substring(0, fileName.length - 4) + "out.smt2")
    if (outFile.exists()) {
      val expected = VerifyTest.runUnwrappingErrors(Script.from(outFile))
      val actual = Script.from(given)
      assertEquals(actual.length, expected.length)
      for((a,b) <- actual zip expected) {

        if(a != b) {
          println("not equal")
          println(a)
          println(b)
        }
        assertEquals(a, b)
      }
    }
  }
}
