package cuvee.test.parser

import java.io.File

import cuvee.Script
import cuvee.Source
import cuvee.time
import minitest.SimpleTestSuite

object CHC2019 extends SimpleTestSuite {
  val path = "chc-comp19-benchmarks"
  val categories = List(
    "lia-lin", "lia-lin-arr", "lia-nonlin", "lra-ts")

  for (cat <- categories) {
    val dir = new File(path + "/" + cat)
    for (file <- dir.list.sorted if file endsWith ".smt2") {
      val benchmark = new File(path + "/" + cat + "/" + file)
      test(file) {
        val source = Source.file(benchmark)
        val (ms, cmds) = time(source.cmds)
        println("parsed: " + file + " (" + ms + "ms)")
      }
    }
  }

  def main(args: Array[String]) {

  }
}