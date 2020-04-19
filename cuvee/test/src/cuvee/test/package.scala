package cuvee

import java.util
import java.util.concurrent.TimeUnit
import java.util.stream.{Collector, Collectors}

import minitest.api

import scala.collection.JavaConverters._

package object test {
  import minitest.SimpleTestSuite

  trait TestSuite extends SimpleTestSuite {

    case object prove_! extends Report {
      def apply(res: Res) = res match {
        case res: IsSat =>
          assertEquals(res, Unsat)
        case any =>
          assertEquals(res, Success)
      }
    }

    def withSolver[T](s: Solver, action: Solver => T): T = {
      try {
        action(s)
      } finally {
        s.exit()
      }
    }

    def listChildProcesses: List[ProcessHandle] = {
      val collector: Collector[ProcessHandle, _, util.List[ProcessHandle]] = Collectors.toList()
      ProcessHandle.current().children().collect(collector).asScala.toList
    }

    var knownUnterminated: Set[ProcessHandle] = Set()

    override def test(name: String)(f: => api.Void): Unit = {
      super.test(name) {
        f
        val childProcesses: List[ProcessHandle] = listChildProcesses filterNot knownUnterminated.contains filter(p => {
          try {
            // wait one second for process to terminate.
            // usually, this would be near-instant because of the asynchronicity
            // or take a whole second because the process wasn't terminated properly
            p.onExit().get(1, TimeUnit.SECONDS)
            false
          } catch {
            case _: Exception =>
              // probably this will not terminate so we will ignore this in the future.
              // otherwise, we'd add a second to and fail all following tests.
              knownUnterminated += p
              true
          }
        })
        assert(childProcesses.isEmpty, "Child processes still alive: " + childProcesses)
      }
    }
  }
}