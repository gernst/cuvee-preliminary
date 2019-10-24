package tacas2020

class Solver {
  var states: List[State] = _
  reset()

  def top = states.head

  def reset() {
    states = List(State.default)
  }

  def exit() {
    System.exit(0)
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

  def map(cmd: Cmd, action: State => State) {
    val st0 = pop()
    val st1 = action(st0)
    val st2 = st1 log cmd
    push(st2)
  }

  def exec(cmds: List[Cmd]) {
    for (cmd <- cmds)
      exec(cmd)
  }

  def exec(cmd: Cmd) = cmd match {
    case Exit =>
      exit()

    case Reset =>
      reset()

    case Push =>
      push(top)

    case Pop =>
      pop

    case GetAssertions =>
      val asserts = top.asserts.reverse
      for (assert <- asserts)
        out("(assert " + assert + ")")

    case Assert(expr) =>
      map(cmd, _ assert expr)

    case DeclareSort(sort, arity) =>
      map(cmd, _ declare (sort, arity))

    case DefineSort(sort, args, body) =>
      map(cmd, _ define (sort, args, body))

    case DeclareFun(id, args, res) =>
      map(cmd, _ declare (id, args, res))

    case DefineFun(id, args, res, body) =>
      map(cmd, _ define (id, args, res, body))
  }
}