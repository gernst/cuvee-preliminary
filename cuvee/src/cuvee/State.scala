package cuvee

case class State(
  sorts: Map[Sort, Int],
  sortdefs: Map[Sort, (List[Sort], Type)],
  datatypes: Map[Sort, Datatype],

  funs: Map[Id, (List[Type], Type)],
  fundefs: Map[Id, (List[Formal], Expr)],

  procs: Map[Id, (List[Type], List[Type])],
  procdefs: Map[Id, Proc],

  objects: Map[Sort, Obj],

  rasserts: List[Expr],
  model: Option[Model]) {

  def asserts = {
    rasserts.reverse
  }

  def withoutAsserts = {
    copy(
      rasserts = Nil,
      model = None)
  }

  def withModel(model: Model) = {
    copy(
      model = Some(model))
  }

  def clearModel = {
    copy(
      model = None)
  }

  def env = {
    val su = funs collect {
      case (id, (Nil, typ)) => (id, id)
    }
    val ty = funs collect {
      case (id, (Nil, typ)) => (id, typ)
    }

    Env(su, ty)
  }

  def const(id: Id): Formal = {
    ensure(funs contains id, "undeclared identifier", id, funs)
    val (args, res) = funs(id)
    ensure(args.isEmpty, "not constant", id)
    Formal(id, res)
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
    copy(
      funs = funs + (id -> (formals, res)),
      fundefs = fundefs + (id -> (formals, body)))
  }

  def define(id: Id, proc: Proc) = {
    ensure(!(procs contains id), "procedure already defined", id)
    Check.checkProc(id, proc)
    val ins: List[Type] = proc.in
    val outs: List[Type] = proc.out
    copy(
      procs = procs + (id -> (ins, outs)),
      procdefs = procdefs + (id -> proc))
  }

  def define(sort: Sort, obj: Obj): State = {
    ensure(!(objects contains sort), "object already defined", sort)
    Check.checkObj(sort, obj)
    copy(
      objects = objects + (sort -> obj))
  }

  def declare(sort: Sort, arity: Int, decl: Datatype): State = {
    var st = declare(sort, arity)
    ensure(arity == decl.params.length, "arity mismatch", arity, decl.params)

    st = st.copy(
      datatypes = datatypes + (sort -> decl))

    for (Constr(id, sels) <- decl.constrs) {
      val args = sels map (_.typ)
      st = st declare (id, args, sort)
      for (Sel(id, typ) <- sels) {
        st = st declare (id, List(sort), typ)
      }
    }

    st
  }

  def declare(arities: List[Arity], decls: List[Datatype]): State = {
    ensure(arities.length == decls.length, "length mismatch", arities, decls)
    var st = this

    for ((Arity(sort, arity), decl) <- (arities zip decls)) {
      st = st declare (sort, arity, decl)
    }

    st
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
    datatypes = Map(),

    funs = Map(
      True -> (List(), Sort.bool),
      False -> (List(), Sort.bool),

      Id.exp -> (List(Sort.int, Sort.int), Sort.int),
      Id.abs -> (List(Sort.int), Sort.int),
      Id.times -> (List(Sort.int, Sort.int), Sort.int),
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
      Id.imp -> (List(Sort.bool, Sort.bool), Sort.bool)

    /* Id.nil -> Fun.nil,
      Id.cons -> Fun.cons,
      Id.in -> Fun.in,
      Id.head -> Fun.head,
      Id.tail -> Fun.tail,
      Id.last -> Fun.last,
      Id.init -> Fun.init,

      Id.select -> Fun.select,
      Id.store -> Fun.store */ ),
    fundefs = Map(),

    procs = Map(),
    procdefs = Map(),

    objects = Map(),

    rasserts = Nil,
    model = None)
}