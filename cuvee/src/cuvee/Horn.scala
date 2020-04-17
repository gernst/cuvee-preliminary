package cuvee

case class Horn(formals: List[Formal], ant /*cedent*/ : List[Expr], suc /*cedent*/ : Expr) {
  override def toString: String = {
    s"(horn\n  ${sexpr(formals)}${ant.map("\n" + Printer.format(_, "  ")).mkString}\n${Printer.format(suc, "  ")})"
  }

  def toExpr: Expr = Forall(formals, Imp(And(ant), suc))
}

object Horn {
  def split(expr: Expr): List[Horn] = expr match {
    case Imp(phi, psi) =>
      val horns = for (Horn(formals, ant, suc) <- split(psi)) yield {
        horn(formals, phi :: ant, suc)
      }
      nonTrivial(horns)
    case And(exprs) =>
      nonTrivial(exprs flatMap split)
    case Forall(formals, expr) =>
      // we can ignore "overwriting" formals here since the outer ones would be unused anyway
      nonTrivial(split(expr) map (h => horn((formals ++ h.formals).distinct, h.ant, h.suc)))
    case _ => nonTrivial(List(horn(Nil, Nil, expr)))
  }

  def nonTrivial(horns: List[Horn]): List[Horn] = horns.filter(h => !h.ant.contains(False) && h.suc != True)

  def horn(formals: List[Formal], ant: List[Expr], suc: Expr): Horn = {
    val ant_ = And.flatten(ant)

    Substitution(formals, ant_, suc) match {
      case Some((ant, suc)) => return horn(formals, ant, suc)
      case _ => // substitution did not change anything
    }

    {
      val (formals_, ant__) = removeExists(formals, ant_, ant_.free -- formals.ids)
      if (ant__ != ant_) {
        return horn(formals_, ant__, suc)
      }
    }

    val ant__ = ant_ map simplify filterNot (_ == True) distinct

    if (ant__ contains suc) {
      return Horn(Nil, Nil, True)
    }

    val free = ant__.free ++ suc.free
    val formals_ = formals.filter(f => free.contains(f.id))
    Horn(formals_, ant__, suc)
  }

  def removeExists(formals: List[Formal], ant: List[Expr], free: Set[Id]): (List[Formal], List[Expr]) = {
    // Why is this so complicated?
    // We need to make sure that we're not accidentally pushing variables from the Exists to the surrounding Forall that are already there.
    // The surrounding Forall changes every time that we remove an Exists.
    ant match {
      case Nil => (formals, Nil)
      case (e@Exists(_, _)) :: rest =>
        val avoid = e.avoid(free ++ formals.ids)
        val Bind(_, locals, body) = e.rename(avoid, avoid)
        val (formals__, ant_) = removeExists(formals ++ locals, rest, free)
        (formals__, body :: ant_)

      case head :: rest =>
        val (formals_, ant_) = removeExists(formals, rest, free)
        (formals_, head :: ant_)
    }
  }

  def simplify(expr: Expr): Expr = expr match {
    case Eq(l, r) if l == r => True
    case Distinct(List(l, r)) if l == r => False
    case other => other
  }

  object Substitution {
    def apply(locals: List[Formal], ant: List[Expr], suc: Expr): Option[(List[Expr], Expr)] = {
      // Special care when substituting global variables: the original equation must be preserved.
      // Make sure that local variables are substituted before globals or opportunities will be missed.
      for (a <- ant) a match {
        case Eq(l: Id, r: Id) if l == r => // removed later. skip or recurse infinitely.

        case Eq(l: Id, r: Expr) if locals.ids contains l =>
          subst(ant, suc, l, r, None) match {
            case r@Some(_) => return r
            case _ => // substitution did not change anything
          }

        case Eq(l: Expr, r: Id) if locals.ids contains r =>
          subst(ant, suc, r, l, None) match {
            case r@Some(_) => return r
            case _ => // substitution did not change anything
          }

        case Eq(l: Id, r: Expr) =>
          subst(ant, suc, l, r, Some(a)) match {
            case r@Some(_) => return r
            case _ => // substitution did not change anything
          }

        case Eq(l: Expr, r: Id) =>
          subst(ant, suc, r, l, Some(a)) match {
            case r@Some(_) => return r
            case _ => // substitution did not change anything
          }

        case _ => // continue
      }
      None
    }

    def subst(ant: List[Expr], suc: Expr, id: Id, expr: Expr, keep: Option[Expr]): Option[(List[Expr], Expr)] = {
      val ant_ = for (e <- ant) yield {
        keep match {
          case Some(f) if e == f => e
          case _ => e.subst(id, expr)
        }
      }

      val suc_ = suc.subst(id, expr)

      if (ant_ != ant || suc_ != suc) Some(ant_, suc_) else None
    }
  }

}

