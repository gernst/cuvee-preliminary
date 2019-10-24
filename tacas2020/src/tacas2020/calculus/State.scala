package tacas2020.calculus

import tacas2020.Error
import tacas2020.syntax._
import tacas2020.pure._

case class State(
  vars: Map[Id, Sort],
  funs: Map[Id, Fun],
  sorts: Set[Id],

  store: Store,
  path: List[Pure],

  old: Option[State],
  simplify: Simplify,
  solver: Solver) {

  override def toString = store.mkString(", ") + " | " + path.mkString(" && ")

  def &&(that: Pure): State = {
    copy(path = that :: path)
  }

  def mark: State = {
    copy(old = Some(this))
  }

  def define(id: Id, fun: Fun): State = {
    copy(funs = funs + (id -> fun))
  }

  def define(fs: Iterable[(Id, Fun)]): State = {
    copy(funs = funs ++ fs)
  }

  def declare(ps: Iterable[(Id, Type)]): State = {
    val qs = ps map resolve
    copy(vars = vars ++ qs)
  }

  def declare(ps: List[Formal]): State = {
    val qs = ps map resolve
    copy(vars = vars ++ qs)
  }

  def arbitrary(id: Id): Var = {
    Var.fresh(id.name, vars(id))
  }

  def arbitrary(param: (Id, Type)): (Id, Var) = {
    val (id, typ) = param
    (id, Var.fresh(id.name, resolve(typ)))
  }

  def arbitrary(ps: Iterable[(Id, Type)]): Map[Id, Var] = {
    val qs = ps map arbitrary
    qs.toMap
  }

  def params(ps: Iterable[Formal]): State = {
    val qs = ps map resolve
    copy(vars = vars ++ qs)
  }

  def assign(x: Id, e: Pure): State = {
    assert(vars contains x)
    copy(store = store + (x -> e))
  }

  def assign(xs: List[Id], es: List[Pure]): State = {
    val unknown = xs filterNot vars.contains
    if (!unknown.isEmpty)
      throw Error("undeclared identifiers", unknown)
    val ps = xs zip es
    copy(store = store ++ ps)
  }

  def havoc(xs: Iterable[Id]): (Map[Id, Var], State) = {
    val ys = xs map { id => (id, Var.fresh(id.name, vars(id))) }
    val st = copy(store = store ++ ys)
    (ys.toMap, st)
  }

  def resolve(param: (Id, Type)): (Id, Sort) = {
    val (id, typ) = param
    (id, resolve(typ))
  }

  def resolve(param: Formal): (Id, Sort) = {
    val Formal(id, typ) = param
    (id, resolve(typ))
  }

  def resolve(typ: Type): Sort = typ match {
    case Type._int => Sort.int
    case Type._boolean => Sort.bool
    case Type.base(id) =>
      // if (!(sorts contains id))
      //   throw error.InvalidProgram("undeclared type: " + id)
      Sort.base(id.name)
    case Type.list(elem) => Sort.list(resolve(elem))
    case Type.set(elem) => Sort.array(resolve(elem), Sort.bool)
    case Type.array(elem) => Sort.array(Sort.int, resolve(elem))
    case Type.map(dom, ran) => Sort.array(resolve(dom), resolve(ran))
  }
}

object State {
  def empty = State(
    vars = Map(),
    funs = Map(),

    sorts = Set(),

    store = Map(),
    path = List(),

    old = None,
    simplify = Simplify.default,
    solver = Solver.default)

  def default = empty copy (
    funs = Map(
      Id.ite -> Fun.ite,

      Id._false -> Fun._false,
      Id._true -> Fun._true,

      Id.exp -> Fun.exp,
      Id.times -> Fun.times,
      Id.divBy -> Fun.divBy,
      Id.mod -> Fun.mod,

      Id.uminus -> Fun.uminus,
      Id.plus -> Fun.plus,
      Id.minus -> Fun.minus,

      Id._eq -> Fun._eq,
      Id.le -> Fun.le,
      Id.lt -> Fun.lt,
      Id.ge -> Fun.ge,
      Id.gt -> Fun.gt,

      Id.not -> Fun.not,
      Id.and -> Fun.and,
      Id.or -> Fun.or,
      Id.imp -> Fun.imp,

      Id.nil -> Fun.nil,
      Id.cons -> Fun.cons,
      Id.in -> Fun.in,
      Id.head -> Fun.head,
      Id.tail -> Fun.tail,
      Id.last -> Fun.last,
      Id.init -> Fun.init,

      Id.select -> Fun.select,
      Id.store -> Fun.store))
}
