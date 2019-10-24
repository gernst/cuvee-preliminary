package tacas2020.pure

import tacas2020.StringOps

case class Fun(name: String, args: List[Sort], ret: Sort) {
  def gen = subst(Sort.fresh(params))
  def params = Set(args flatMap (_.free): _*)
  def ren(re: TRen) = Fun(name, args map (_ rename re), ret rename re)
  def subst(env: Typing) = Fun(name, args map (_ subst env), ret subst env)
  def apply(args: Pure*) = App(this, args.toList)
  override def toString = name.toString
}

object Fun {
  val ite = Fun("ite", List(Sort.bool, Param.alpha, Param.alpha), Param.alpha)

  val _true = Fun("true", List(), Sort.bool)
  val _false = Fun("false", List(), Sort.bool)
  
  val exp = Fun("^", List(Sort.int, Sort.int), Sort.int)
  val times = Fun("*", List(Sort.int, Sort.int), Sort.int)
  val divBy = Fun("/", List(Sort.int, Sort.int), Sort.int)
  val mod = Fun("%", List(Sort.int, Sort.int), Sort.int)

  val uminus = Fun("-", List(Sort.int), Sort.int)
  val plus = Fun("+", List(Sort.int, Sort.int), Sort.int)
  val minus = Fun("-", List(Sort.int, Sort.int), Sort.int)

  val _eq = Fun("=", List(Param.alpha, Param.alpha), Sort.bool)
  val le = Fun("<=", List(Sort.int, Sort.int), Sort.bool)
  val lt = Fun("<", List(Sort.int, Sort.int), Sort.bool)
  val ge = Fun(">=", List(Sort.int, Sort.int), Sort.bool)
  val gt = Fun(">", List(Sort.int, Sort.int), Sort.bool)

  val not = Fun("not", List(Sort.bool), Sort.bool)
  val and = Fun("and", List(Sort.bool, Sort.bool), Sort.bool)
  val or = Fun("or", List(Sort.bool, Sort.bool), Sort.bool)
  val imp = Fun("=>", List(Sort.bool, Sort.bool), Sort.bool)

  val nil = Fun("nil", List(), Param.list)
  val cons = Fun("cons", List(Param.alpha, Param.list), Param.list)
  val in = Fun("in", List(Param.alpha, Param.list), Sort.bool)
  val head = Fun("head", List(Param.list), Param.alpha)
  val tail = Fun("tail", List(Param.list), Param.list)
  val last = Fun("last", List(Param.list), Param.alpha)
  val init = Fun("init", List(Param.list), Param.list)

  val select = Fun("select", List(Param.array, Param.alpha), Param.beta)
  val store = Fun("store", List(Param.array, Param.alpha, Param.beta), Param.array)
}

