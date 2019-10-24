package tacas2020.pure

object Solver {
  var timeout = 5000
  var uninterpreted: Set[Fun] = Set()
  def default = SMT2.z3(timeout)
}

trait Solver {
  def assume(phi: Pure)
  def assumeDistinct(exprs: Iterable[Pure])
  def push()
  def pop()

  def isConsistent: Boolean

  def isSatisfiable(phi: Pure): Boolean = {
    assuming(phi) { isConsistent }
  }

  def assume(phis: Iterable[Pure]) {
    for (phi <- phis)
      assume(phi)
  }

  def scoped[A](f: => A): A = {
    push()
    try {
      f
    } finally {
      pop()
    }
  }

  def assuming[A](phis: Pure*)(f: => A): A = scoped {
    assume(phis)
    f
  }

  def isValid(phi: Pure): Boolean = {
    !isSatisfiable(!phi)
  }
}
