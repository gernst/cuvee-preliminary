package cuvee

trait Solver {
  def exec(cmd: Cmd): Option[Res]
}

trait Dispatch {
  this: Solver =>

  def setLogic(logic: String): Ack
  def setOption(args: List[String]): Ack

  def reset(): Unit
  def push(): Unit
  def pop(): Unit
  def exit(): Nothing

  def check(): IsSat
  def assertions(): Assertions
  def model(): Model

  def declare(sort: Sort, arity: Int): Ack
  def define(sort: Sort, args: List[Sort], body: Type): Ack

  def declare(id: Id, args: List[Type], res: Type): Ack
  def define(id: Id, formals: List[Formal], res: Type, body: Expr, rec: Boolean): Ack

  def assert(expr: Expr): Ack

  def exec(cmd: Cmd): Option[Res] = cmd match {
    case SetLogic(logic) =>
      Some(setLogic(logic))
    case SetOption(args) =>
      Some(setOption(args))

    case Reset =>
      reset(); None
    case Push =>
      push(); None
    case Pop =>
      pop(); None
    case Exit =>
      exit()

    case CheckSat =>
      Some(check())
    case GetAssertions =>
      Some(assertions())
    case GetModel =>
      Some(model())

    case DeclareSort(sort, arity) =>
      Some(declare(sort, arity))
    case DefineSort(sort, args, body) =>
      Some(define(sort, args, body))

    case DeclareFun(id, args, res) =>
      Some(declare(id, args, res))
    case DefineFun(id, formals, res, body) =>
      Some(define(id, formals, res, body, false))
    case DefineFunRec(id, formals, res, body) =>
      Some(define(id, formals, res, body, true))

    case Assert(expr) =>
      Some(assert(expr))
  }
}

