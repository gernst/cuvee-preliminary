package tacas2020

case class State(
  sorts: Map[Sort, Int],
  sortdefs: Map[Sort, (List[Sort], Type)],

  consts: Map[Id, Type],
  funs: Map[Id, (List[Type], Type)],
  fundefs: Map[Id, (List[Id], Expr)],

  asserts: List[Expr]) {

  def declare(sort: Sort, arity: Int) = {
    copy(
      sorts = sorts + (sort -> arity))
  }

  def define(sort: Sort, args: List[Sort], body: Type) = {
    val arity = args.length
    copy(
      sorts = sorts + (sort -> arity),
      sortdefs = sortdefs + (sort -> (args, body)))
  }

  def declare(id: Id, res: Type) = {
    copy(
      consts = consts + (id -> res))
  }

  def declare(id: Id, args: List[Type], res: Type) = {
    copy(
      funs = funs + (id -> (args, res)))
  }

  def define(id: Id, formals: List[Formal], res: Type, body: Expr) = {
    val args = formals map (_.typ)
    val ids = formals map (_.id)
    copy(
      funs = funs + (id -> (args, res)),
      fundefs = fundefs + (id -> (ids, body)))
  }

  def assert(expr: Expr) = {
    copy(
      asserts = expr :: asserts)
  }
}

object State {
  def default = State(
    sorts = Map(
      Sort.bool -> 0,
      Sort.int -> 0),
    sortdefs = Map(),

    consts = Map(
      True -> Sort.bool,
      False -> Sort.bool),

    funs = Map(),
    fundefs = Map(),

    asserts = Nil)
}