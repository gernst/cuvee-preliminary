package cuvee.test

import cuvee._

import minitest.SimpleTestSuite
import java.io.OutputStream
import java.io.PrintStream
import java.io.File

object Examples extends SimpleTestSuite {
  val dir = "examples"

  val tests = List(
    "add.smt2",
    "gcd.smt2",
    "map.smt2",
    "max.smt2",
    "compare.smt2")

  for (file <- tests if file.endsWith(".smt2")) {
    val path = dir + "/" + file
    test(path) {
      val buf = new StringBuilder
      object stream extends OutputStream {
        def write(b: Int) { buf append b.toChar }
      }
      Main._out = new PrintStream(stream)

      Main.run(List(path, "-z3"))

      val result = buf.toString.trim
      assertEquals(result, "unsat")
    }
  }

  def main(args: Array[String]) {
  }
}