package tacas2020

class Solver {
  var states: List[State] = _
  reset()

  def top = states.head

  def reset() {
    states = List(State.default)
  }

  def exit(): Nothing = {
    System.exit(0)
    ??? // unreachable
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

  def ack() = {
    List()
  }

  def map(cmd: Cmd, action: State => State) {
    val st0 = pop()
    try {
      val st1 = action(st0)
      val st2 = st1 log cmd
      push(st2)
    } catch {
      case e: Throwable =>
        push(st0)
        throw e
    }
  }

  def exec(cmds: List[Cmd]): List[String] = {
    cmds flatMap exec
  }

  def exec(cmd: Cmd): List[String] = cmd match {
    case Exit =>
      exit()

    case Reset =>
      reset()
      ack()

    case Push =>
      push(top)
      ack()

    case Pop =>
      pop()
      ack()

    case GetAssertions =>
      val asserts = top.asserts.reverse
      for (assert <- asserts)
        yield "(assert " + assert + ")"

    case CheckSat =>
      map(cmd, x => x)
      ack()

    case Assert(expr) =>
      import Eval.eval
      val _expr = eval(expr, top.env, List.empty, top)
      val _cmd = Assert(_expr)
      map(_cmd, _ assert _expr)
      ack()

    case DeclareSort(sort, arity) =>
      map(cmd, _ declare (sort, arity))
      ack()

    case DefineSort(sort, args, body) =>
      map(cmd, _ define (sort, args, body))
      ack()

    case DeclareFun(id, args, res) =>
      map(cmd, _ declare (id, args, res))
      ack()

    case DefineFun(id, args, res, body) =>
      map(cmd, _ define (id, args, res, body))
      ack()
  }
}