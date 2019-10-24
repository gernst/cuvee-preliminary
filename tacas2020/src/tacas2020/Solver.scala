package tacas2020

class Solver {
  var states: List[State] = _
  reset()

  val top = states.head

  def reset() {
    states = List(State.default)
  }

  def pop() = {
    ensure(!states.isEmpty, "empty stack")
    val st :: rest = states
    states = rest
    st
  }

  def push(st: State) {
    states = st :: states
  }

  def exec(cmd: Cmd) = cmd match {
    case Exit =>
      System.exit(0)

    case Reset =>
      reset()

    case Push =>
      push(top)

    case Pop =>
      pop

    case Assert(expr) =>
      val state = pop()
      push(state assert expr)

    case DeclareSort(sort, arity) =>
      val state = pop()
      push(state declare (sort, arity))

    case DefineSort(sort, args, body) =>
      val state = pop()
      push(state define (sort, args, body))

    case DeclareFun(id, Nil, res) =>
      val state = pop()
      push(state declare (id, res))

    case DeclareFun(id, args, res) =>
      val state = pop()
      push(state declare (id, args, res))

    case DefineFun(id, args, res, body) =>
      val state = pop()
      push(state define (id, args, res, body))
  }
}