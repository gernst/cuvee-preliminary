package tacas2020

import scala.language.implicitConversions
import java.io.InputStreamReader
import java.io.FileReader
import java.io.Reader
import java.io.File

package object syntax {
  import pure.Pure

  type Ren = Map[Id, Id]
  type Subst = Map[Id, Expr]
  type Store = Map[Id, Pure]
  type Typing = Map[Id, Type]

  val Skip = Block()

  val True = Call("true")
  val False = Call("false")

  implicit def toExpr(int: Int) = Lit(int)
}