package tacas2020.pure

import tacas2020.Alpha

sealed trait Sort extends Sort.term {
  def free: Set[Param]
  def rename(re: TRen): Sort
  def subst(ty: Typing): Sort
}

case class Param(name: String, index: Option[Int] = None) extends Sort with Sort.x {
  def fresh(index: Int) = Param(name, Some(index))
  override def toString = "'" + name // Note: does not leak to SMT solver
}

object Param {
  val alpha = Param("a")
  val beta = Param("b")

  val list = Sort.list(alpha)
  val array = Sort.array(alpha, beta)
}

object Sort extends Alpha[Sort, Param] {
  val bool = base("bool")
  val int = base("int")

  case class base(name: String) extends Sort {
    def free = Set()
    def rename(re: TRen) = this
    def subst(ty: Typing) = this
    override def toString = name
  }

  case class array(dom: Sort, ran: Sort) extends Sort {
    def free = dom.free ++ ran.free
    def rename(re: TRen) = array(dom rename re, ran rename re)
    def subst(ty: Typing) = array(dom subst ty, ran subst ty)
    override def toString = "(Array " + dom + " " + ran + ")"
  }

  case class list(elem: Sort) extends Sort {
    def free = elem.free
    def rename(re: TRen) = list(elem rename re)
    def subst(ty: Typing) = list(elem subst ty)
    override def toString = "(List " + elem + ")"
  }

  def unify(pats: List[Sort], args: List[Sort], nongen: Set[Param], env: Typing): Typing = (pats, args) match {
    case (Nil, Nil) =>
      env
    case (pat :: pats, arg :: args) =>
      unify(pats, args, nongen, unify(pat, arg, nongen, env))
    case _ =>
      assert(false, "ill-typed: " + pats + " mismatches " + args)
      env
  }

  def unify(pat: Sort, arg: Sort, nongen: Set[Param], env: Typing): Typing = (pat, arg) match {
    case (p: Param, _) if (env contains p) && (env(p) == arg) =>
      env
    case (p: Param, _) if !(nongen contains p) =>
      env + (p -> arg)
    case (p: Param, _) =>
      assert(pat == arg, "ill-typed: non-generic " + pat + " cannot be instantiated with " + arg)
      env
    case (_, p: Param) =>
      unify(p, arg, nongen, env)
    case (pat: Sort, arg: Sort) if pat == arg =>
      env
    case (array(patdom, patran), array(argdom, argran)) =>
      unify(patran, argran, nongen, unify(patdom, argdom, nongen, env))
    case (list(pat), list(arg)) =>
      unify(pat, arg, nongen, env)
    case _ =>
      assert(pat == arg, "ill-typed: " + pat + " mismatches " + arg)
      env
  }
}