import mill._
import mill.scalalib._
import mill.scalalib.publish._

object tacas2020 extends ScalaModule {
  def scalaVersion = "2.12.8"

  def mainClass = Some("tacas2020.Main")

  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.1.7")

  object test extends Tests {
    def ivyDeps = Agg(ivy"io.monix::minitest:2.7.0")
    def testFrameworks = Seq("minitest.runner.Framework")
    def unmanagedClasspath = tacas2020.unmanagedClasspath
  }

  def unmanagedClasspath = T {
    if (!ammonite.ops.exists(millSourcePath / "lib")) Agg()
    else Agg.from(ammonite.ops.ls(millSourcePath / "lib").map(PathRef(_)))
  }
}
