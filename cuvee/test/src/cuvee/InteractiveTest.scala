package cuvee

object InteractiveTest {
  def run(args: String*) = {
    Cuvee.run(args.toList)
  }
  def main(args: Array[String]) {
    // run("-simplify", "examples/loops.smt2")
    run("-no-simplify", "examples/loops.smt2")
  }
}