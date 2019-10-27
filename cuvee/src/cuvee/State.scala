package cuvee

case class State(
  sorts: Map[Sort, Int],
  sortdefs: Map[Sort, (List[Sort], Type)],

  funs: Map[Id, (List[Type], Type)],
  fundefs: Map[Id, (List[Id], Expr)],

  rasserts: List[Expr]) {
  
  def asserts = rasserts.reverse
  
  def env = {
    val su = funs collect {
      case (id, (Nil, typ)) => (id, id)
    }
    val ty = funs collect {
      case (id, (Nil, typ)) => (id, typ)
    }
    
    Env(su, ty)
  }
  
  def declare(sort: Sort, arity: Int) = {
    ensure(!(sorts contains sort), "sort already defined", sort)
    copy(
      sorts = sorts + (sort -> arity))
  }

  def define(sort: Sort, args: List[Sort], body: Type) = {
    ensure(!(sorts contains sort), "sort already defined", sort)
    val arity = args.length
    copy(
      sorts = sorts + (sort -> arity),
      sortdefs = sortdefs + (sort -> (args, body)))
  }

  def declare(id: Id, args: List[Type], res: Type) = {
    ensure(!(funs contains id), "function already defined", id)
    copy(
      funs = funs + (id -> (args, res)))
  }

  def define(id: Id, formals: List[Formal], res: Type, body: Expr) = {
    ensure(!(funs contains id), "const already defined", id)
    val args = formals map (_.typ)
    val ids = formals map (_.id)
    copy(
      funs = funs + (id -> (args, res)),
      fundefs = fundefs + (id -> (ids, body)))
  }
  
  def assert(expr: Expr) = {
    copy(
      rasserts = expr :: rasserts)
  }
}

object State {
  def default = State(
    sorts = Map(
      Sort.bool -> 0,
      Sort.int -> 0),
    sortdefs = Map(),

    funs = Map(
      True -> (List(), Sort.bool),
      False -> (List(), Sort.bool),

      Id.exp -> (List(Sort.int, Sort.int), Sort.int),
      Id.times ->(List(Sort.int, Sort.int), Sort.int),
      Id.divBy -> (List(Sort.int, Sort.int), Sort.int),
      Id.mod -> (List(Sort.int, Sort.int), Sort.int),

      Id.uminus -> (List(Sort.int), Sort.int),
      Id.plus -> (List(Sort.int, Sort.int), Sort.int),
      Id.minus -> (List(Sort.int, Sort.int), Sort.int),

      Id.le -> (List(Sort.int, Sort.int), Sort.bool),
      Id.lt -> (List(Sort.int, Sort.int), Sort.bool),
      Id.ge -> (List(Sort.int, Sort.int), Sort.bool),
      Id.gt -> (List(Sort.int, Sort.int), Sort.bool),

      Id.not -> (List(Sort.bool), Sort.bool),
      Id.and -> (List(Sort.bool, Sort.bool), Sort.bool),
      Id.or -> (List(Sort.bool, Sort.bool), Sort.bool),
      Id.imp -> (List(Sort.bool, Sort.bool), Sort.bool),

      /* Id.nil -> Fun.nil,
      Id.cons -> Fun.cons,
      Id.in -> Fun.in,
      Id.head -> Fun.head,
      Id.tail -> Fun.tail,
      Id.last -> Fun.last,
      Id.init -> Fun.init,

      Id.select -> Fun.select,
      Id.store -> Fun.store */),
    fundefs = Map(),

    rasserts = Nil)
}