package cuvee

trait Assoc {
  def reduce[A](xs: Iterable[A], f: (A, A) => A): A
  def fold[A](xs: Iterable[A], z: A, f: (A, A) => A): A
}

object Assoc {
  // Note: nested to avoid name clashes with Either cases
  case object left extends Assoc {
    def reduce[A](xs: Iterable[A], f: (A, A) => A): A = xs.reduceLeft(f)
    def fold[A](xs: Iterable[A], z: A, f: (A, A) => A): A = xs.foldLeft(z)(f)
  }

  case object right extends Assoc {
    def reduce[A](xs: Iterable[A], f: (A, A) => A): A = xs.reduceRight(f)
    def fold[A](xs: Iterable[A], z: A, f: (A, A) => A): A = xs.foldRight(z)(f)
  }
}

object Sugar {
  class unary(val fun: Id) extends (Expr => Expr) {
    def unapply(expr: Expr) = expr match {
      case App(`fun`, List(arg)) => Some(arg)
      case _ => None
    }

    def apply(arg: Expr) = {
      App(fun, List(arg))
    }
  }

  class binary(val fun: Id) extends ((Expr, Expr) => Expr) {
    def unapply(expr: Expr) = expr match {
      case App(`fun`, List(arg1, arg2)) => Some((arg1, arg2))
      case _ => None
    }

    def apply(arg1: Expr, arg2: Expr) = {
      App(fun, List(arg1, arg2))
    }

    def left(args: List[Expr]) = {
      args reduceLeft this
    }

    def right(args: List[Expr]) = {
      args reduceRight this
    }
  }

  case class commutative(val fun: Id, val neutral: Expr, val assoc: Assoc) extends (List[Expr] => Expr) {
    def flatten(exprs: List[Expr]): List[Expr] = {
      exprs flatMap flatten
    }

    def flatten(expr: Expr): List[Expr] = expr match {
      case App(`fun`, args) => flatten(args)
      case _ => List(expr)
    }

    def apply(arg1: Expr, arg2: Expr): Expr = {
      App(fun, List(arg1, arg2))
    }

    def apply(args: List[Expr]): Expr = args match {
      case Nil => neutral
      case List(arg) => arg
      case _ => assoc.reduce(args, apply)
    }

    def unapply(expr: Expr) = expr match {
      case App(`fun`, args) =>
        Some(flatten(args))
      case _ =>
        None
    }
  }
}