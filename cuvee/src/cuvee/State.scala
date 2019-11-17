package cuvee

case class State(
  sorts: Map[Sort, Int],
  sortdefs: Map[Sort, (List[Sort], Type)],

  funs: Map[Id, (List[Type], Type)],
  fundefs: Map[Id, (List[Id], Expr)],

  rasserts: List[Expr],
  model: Option[Model]) {
  
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
  
  def declare(sort: Sort, arity: Int): State = {
    ensure(!(sorts contains sort), "sort already defined", sort)
    copy(
      sorts = sorts + (sort -> arity))
  }

  def define(sort: Sort, args: List[Sort], body: Type): State = {
    ensure(!(sorts contains sort), "sort already defined", sort)
    val arity = args.length
    copy(
      sorts = sorts + (sort -> arity),
      sortdefs = sortdefs + (sort -> (args, body)))
  }

  def declare(id: Id, args: List[Type], res: Type): State = {
    ensure(!(funs contains id), "function already defined", id)
    copy(
      funs = funs + (id -> (args, res)))
  }

  def define(id: Id, formals: List[Formal], res: Type, body: Expr): State = {
    ensure(!(funs contains id), "const already defined", id)
    val args = formals map (_.typ)
    val ids = formals map (_.id)
    copy(
      funs = funs + (id -> (args, res)),
      fundefs = fundefs + (id -> (ids, body)))
  }
  
  def declare(sort: Sort, arity: Int, decl: Datatype): State = {
    val st = declare(sort, arity)
    ensure(arity == decl.params.length, "arity mismatch", arity, decl.params)
      
    decl.constrs.foldLeft(st) {
      case (st, Constr(id, sels)) =>
        val args = sels map (_.typ)
        val st_ = st declare (id, args, sort)
        sels.foldLeft(st_) {
          case (st, Sel(id, typ)) =>
            st declare (id, List(sort), typ)
        }
    }
  }
  
  def declare(arities: List[Arity], decls: List[Datatype]): State = {
    ensure(arities.length == decls.length, "length mismatch", arities, decls)
    (arities zip decls).foldLeft(this) {
      case (st, (Arity(sort, arity), decl)) =>
        st declare(sort, arity, decl)
    }
  }
  
  def assert(expr: Expr) = {
    copy(
      rasserts = expr :: rasserts)
  }
  
  def withModel(model: Model) = {
    copy(
      model = Some(model))
  }
  
  def clearModel = {
    copy(
      model = None)
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

    rasserts = Nil,
    model = None)
}