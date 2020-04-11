import mill._
import mill.scalalib._
import mill.scalalib.publish._

object cuvee extends ScalaModule {
  def scalaVersion = "2.12.11"
  def forkArgs = Seq("-Xss32m")

  def mainClass = Some("cuvee.Cuvee")

  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.2.0")

  object test extends Tests {
    def forkArgs = Seq("-Xss32m")
    def ivyDeps = Agg(ivy"io.monix::minitest:2.7.0")
    def testFrameworks = Seq("minitest.runner.Framework")
    def unmanagedClasspath = cuvee.unmanagedClasspath
  }

  def unmanagedClasspath = T {
    if (!ammonite.ops.exists(millSourcePath / "lib")) Agg()
    else Agg.from(ammonite.ops.ls(millSourcePath / "lib").map(PathRef(_)))
  }
}
