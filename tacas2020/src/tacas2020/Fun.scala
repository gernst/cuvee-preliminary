package tacas2020

case class Fun(name: String, args: List[Sort], ret: Sort, fixity: Fixity = Nilfix) {
  def gen = subst(Sort.fresh(params))
  def params = Set(args flatMap (_.free): _*)
  def ren(re: TRen) = Fun(name, args map (_ rename re), ret rename re, fixity)
  def subst(env: Typing) = Fun(name, args map (_ subst env), ret subst env, fixity)
  def apply(args: Pure*) = App(this, args.toList)
  override def toString = name.toString

  // TODO: add parens only when necessary
  def format(args: List[Any], prec: Int, assoc: Assoc): String = (fixity, args) match {
    case (Nilfix, Nil) =>
      toString
    // case (_, List(arg1, arg2)) if this == Pred._eq(Sort.bool) =>
    //   "(" + arg1 + " <=> " + arg2 + ")"
    //    case (_, List(Eq(arg1, arg2))) =>
    //      "(" + arg1 + " != " + arg2 + ")"
    case (Formfix, args) =>
      name form args
    case (_: Prefix, List(arg)) =>
      this + " " + arg
    case (_: Postfix, List(arg)) =>
      arg + " " + this
    case (_: Infix, List(arg1, arg2)) =>
      "(" + arg1 + " " + this + " " + arg2 + ")"
    case _ =>
      this + args.mkString("(", ", ", ")")
  }
}

object Fun {
  val ite = Fun("_?_:_", List(Sort.bool, Param.alpha, Param.alpha), Param.alpha, Formfix)

  val unit = Fun("unit", List(), Sort.unit)
  val _true = Fun("true", List(), Sort.bool)
  val _false = Fun("false", List(), Sort.bool)
  
  val exp = Fun("^", List(Sort.int, Sort.int), Sort.int, Infix(Left, 9))
  val times = Fun("*", List(Sort.int, Sort.int), Sort.int, Infix(Left, 8))
  val divBy = Fun("/", List(Sort.int, Sort.int), Sort.int, Infix(Non, 8))
  val mod = Fun("%", List(Sort.int, Sort.int), Sort.int, Infix(Non, 8))

  val uminus = Fun("-", List(Sort.int), Sort.int, Prefix(8))
  val plus = Fun("+", List(Sort.int, Sort.int), Sort.int, Infix(Left, 7))
  val minus = Fun("-", List(Sort.int, Sort.int), Sort.int, Infix(Left, 7))

  val _eq = Fun("==", List(Param.alpha, Param.alpha), Sort.bool, Infix(Non, 6))
  val le = Fun("<=", List(Sort.int, Sort.int), Sort.bool, Infix(Non, 6))
  val lt = Fun("<", List(Sort.int, Sort.int), Sort.bool, Infix(Non, 6))
  val ge = Fun(">=", List(Sort.int, Sort.int), Sort.bool, Infix(Non, 6))
  val gt = Fun(">", List(Sort.int, Sort.int), Sort.bool, Infix(Non, 6))

  val not = Fun("!", List(Sort.bool), Sort.bool, Prefix(5))
  val and = Fun("&&", List(Sort.bool, Sort.bool), Sort.bool, Infix(Left, 4))
  val or = Fun("||", List(Sort.bool, Sort.bool), Sort.bool, Infix(Left, 3))
  val imp = Fun("==>", List(Sort.bool, Sort.bool), Sort.bool, Infix(Right, 2))
  val eqv = Fun("<=>", List(Sort.bool, Sort.bool), Sort.bool, Infix(Non, 1))

  val nil = Fun("nil", List(), Param.list)
  val cons = Fun("cons", List(Param.alpha, Param.list), Param.list)
  val in = Fun("in", List(Param.alpha, Param.list), Sort.bool)
  val head = Fun("head", List(Param.list), Param.alpha)
  val tail = Fun("tail", List(Param.list), Param.list)
  val last = Fun("last", List(Param.list), Param.alpha)
  val init = Fun("init", List(Param.list), Param.list)

  val select = Fun("_[_]", List(Param.array, Param.alpha), Param.beta, Formfix)
  val store = Fun("_[_:=_]", List(Param.array, Param.alpha, Param.beta), Param.array, Formfix)
}

