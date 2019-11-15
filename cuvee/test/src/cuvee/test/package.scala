package cuvee

package object test {
  import minitest.SimpleTestSuite
  import cuvee._

  trait TestSuite extends SimpleTestSuite {

    case object prove_! extends Report {
      def apply(res: Res) = res match {
        case res: IsSat =>
          assertEquals(res, Unsat)
        case _ =>
      }
    }
  }
}