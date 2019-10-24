package tacas2020

package object pure {
  type TRen = Map[Param, Param]

  type Typing = Map[Param, Sort]
  object Typing { val empty: Typing = Map() }

  type Ren = Map[Var, Var]
  object Ren { val empty: Ren = Map() }

  type Subst = Map[Var, Pure]
  object Subst { val empty: Subst = Map() }

  val True = Fun._true()
  val False = Fun._false()

  implicit def toConst(n: Int) = Const.int(n)
}