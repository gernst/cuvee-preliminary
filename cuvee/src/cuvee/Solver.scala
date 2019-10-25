package cuvee

class Solver {
  var rlog: List[Cmd] = Nil
  var states: List[State] = _
  reset()

  override def toString = {
    rlog.reverse.mkString("\n")
  }

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

  def ack(cmd: Cmd) = {
    rlog = cmd :: rlog
    List()
  }

  def map(action: State => State) {
    val st0 = pop()
    try {
      push(action(st0))
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
      ack(cmd)

    case Push =>
      push(top)
      ack(cmd)

    case Pop =>
      pop()
      ack(cmd)

    case GetModel =>
      ack(cmd)

    case GetAssertions =>
      val asserts = top.asserts.reverse
      for (assert <- asserts)
        yield "(assert " + assert + ")"

    case CheckSat =>
      map(x => x)
      ack(cmd)

    case SetLogic(logic) =>
      map(x => x)
      ack(cmd)

    case Assert(expr) =>
      import Eval.eval
      val _expr = eval(expr, top.env, List.empty, top)
      val _cmd = Assert(_expr)
      map(_ assert _expr)
      ack(_cmd)

    case DeclareSort(sort, arity) =>
      map(_ declare (sort, arity))
      ack(cmd)

    case DefineSort(sort, args, body) =>
      map(_ define (sort, args, body))
      ack(cmd)

    case DeclareFun(id, args, res) =>
      map(_ declare (id, args, res))
      ack(cmd)

    case DefineFun(id, args, res, body) =>
      map(_ define (id, args, res, body))
      ack(cmd)

    case DefineFunRec(id, args, res, body) =>
      map(_ define (id, args, res, body))
      ack(cmd)
  }
}