package cuvee

trait Associativity
case object Left extends Associativity
case object Right extends Associativity

object Sugar {
  class nullary(val fun: Id) {
    def unapply(expr: Expr) = expr match {
      case App(`fun`, List()) => Some(())
      case _ => None
    }

    def apply() = {
      App(fun, List())
    }
  }

  class unary(val fun: Id) extends (Expr => Expr) {
    def unapply(expr: Expr) = expr match {
      case App(`fun`, List(arg)) => Some(arg)
      case _ => None
    }

    def apply(arg: Expr) = {
      App(fun, List(arg))
    }
  }

  class binary(val fun: Id, val comm: Boolean = true, val assoc: Associativity = Left) extends ((Expr, Expr) => Expr) {
    def unapply(expr: Expr) = expr match {
      case App(`fun`, List(arg1, arg2)) => Some((arg1, arg2))
      case _ => None
    }

    def apply(arg1: Expr, arg2: Expr) = {
      App(fun, List(arg1, arg2))
    }

    def flatten(exprs: List[Expr]): List[Expr] = {
      (comm, assoc, exprs) match {
        case (true, _, _) =>
          exprs flatMap flatten
        case (_, Left, List(left, right)) =>
          flatten(left) ++ List(right)
        case (_, Right, List(left, right)) =>
          left :: flatten(right)
      }
    }

    private def flatten(expr: Expr): List[Expr] = expr match {
      case App(`fun`, args) =>
        flatten(args)
      case _ =>
        List(expr)
    }

    object nary extends (List[Expr] => App) {
      def apply(args: List[Expr]) = (fun, args, assoc) match {
        case (Id.minus, List(unary), _) =>
          App(fun, List(Num(0), unary))
        case (_, fst :: snd :: (rest @ (_ :: _)), Left) =>
          App(fun, App(fun, fst, snd) :: rest)
        case (_, fst :: snd :: (rest @ (_ :: _)), Right) =>
          App(fun, List(fst, nary(snd :: rest)): List[Expr])
        case _ =>
          App(fun, args)
      }

      def unapply(expr: Expr) = expr match {
        case App(`fun`, List(left, right)) if fun == Id.minus && left == Num(0) =>
          Some(List(right))
        case App(`fun`, args) =>
          Some(flatten(args))
        case _ =>
          None
      }
    }
  }

  class ternary(val fun: Id) extends ((Expr, Expr, Expr) => Expr) {
    def unapply(expr: Expr) = expr match {
      case App(`fun`, List(arg1, arg2, arg3)) => Some((arg1, arg2, arg3))
      case _ => None
    }

    def apply(arg1: Expr, arg2: Expr, arg3: Expr): Expr = {
      App(fun, List(arg1, arg2, arg3))
    }
  }
}