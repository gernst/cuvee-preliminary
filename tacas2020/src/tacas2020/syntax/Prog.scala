package tacas2020.syntax

import tacas2020.pure._

sealed trait Prog {
  def mod: Set[Id]
  def read: Set[Id]
  def rename(re: Ren): Prog
}

case class Block(progs: List[Prog]) {
  def mod = Set(progs flatMap (_.mod): _*)
  def read = Set(progs flatMap (_.read): _*)
  def rename(re: Ren) = Block(progs map (_ rename re))
  def open = progs

  def ++(that: Block) = Block(this.progs ++ that.progs)
  override def toString = progs mkString ("{ ", "; ", " }")
}

object Block {
  def apply(progs: Prog*): Block = {
    Block(progs.toList)
  }
}

case class Assign(xs: List[Id], es: List[Expr]) extends Prog {
  def mod = xs.toSet
  def read = Set(es flatMap (_.free): _*)
  def rename(re: Ren) = Assign(xs map (_ rename re), es map (_ rename re))
  override def toString = xs.mkString(", ") + " := " + es.mkString(", ")
}

object Assign {
  def apply(x: Id, e: Expr): Assign = {
    Assign(List(x), List(e))
  }

  def apply(xes: List[(Id, Expr)]): Assign = {
    val (xs, es) = xes.unzip
    Assign(xs, es)
  }
}

case class Let(ps: List[Formal], es: List[Expr]) extends Prog {
  def xs = ps map (_.ident)
  def mod = xs.toSet
  def read = Set(es flatMap (_.free): _*)
  def rename(re: Ren) = Let(ps map (_ rename re), es map (_ rename re))
  override def toString = "let " + ps.mkString(", ") + " := " + es.mkString(", ")
}

object Let {
  def apply(p: Formal, e: Expr): Let = {
    Let(List(p), List(e))
  }
}

case class Spec(pre: Expr, mod: Set[Id], post: Expr) extends Prog {
  def read = pre.free ++ (post.free -- mod)
  def rename(re: Ren) = Spec(pre rename re, mod map (_ rename re), post rename re)
  override def toString = "[" + pre + mod.mkString("; ", ", ", ": ") + post + "]"
}

case class If(test: Expr, left: Block, right: Block) extends Prog {
  def mod = left.mod ++ right.mod
  def read = test.free ++ left.read ++ right.read
  def rename(re: Ren) = If(test rename re, left rename re, right rename re)
  override def toString = "if " + test + " then " + left + " else " + right
}

object If {
  def apply(test: Expr, left: Prog*): If = {
    If(test, Block(left: _*), Skip)
  }

  def apply(test: Expr, left: Block): If = {
    If(test, left, Skip)
  }

  def apply(test: Expr, left: Prog, right: Prog): If = {
    If(test, Block(left), Block(right))
  }

  def apply(conds: (Expr, Prog)*): Block = {
    conds.foldRight(Skip) {
      case ((phi, prog), rest) =>
        Block(If(phi, Block(prog), rest))
    }
  }
}

case class While(test: Expr, body: Block) {
  def mod = body.mod
  def read = test.free ++ body.read
  def rename(re: Ren) = While(test rename re, body rename re)
  override def toString = "while " + test + " do " + body
}

object While {
  def apply(test: Expr, body: Prog*): While = {
    While(test, Block(body: _*))
  }
}

case class WhileInvariant(inv: Expr, term: Option[Expr], loop: While) extends Prog {
  def test = loop.test
  def body = loop.body
  def mod = loop.mod
  def read = inv.free ++ loop.read
  def rename(re: Ren) = WhileInvariant(inv rename re, term map (_ rename re), loop rename re)
  override def toString = "while " + test + " do " + body
}

object WhileInvariant {
  def apply(inv: Expr, term: Expr, loop: While): WhileInvariant = {
    WhileInvariant(inv, Some(term), loop)
  }
}

case class WhileContract(pre: Expr, term: Option[Expr], loop: While, second: Block, post: Expr, proof: Block) extends Prog {
  def test = loop.test
  def body = loop.body
  def mod = loop.mod ++ second.mod
  def read = pre.free ++ loop.read ++ second.read ++ post.free
  def rename(re: Ren) = WhileContract(pre rename re, term map (_ rename re), loop rename re, second rename re, post rename re, proof rename re)
  override def toString = "while " + test + " do " + body
}

object WhileContract {
  def apply(pre: Expr, term: Expr, first: While, post: Expr): WhileContract = {
    WhileContract(pre, Some(term), first, Skip, post, Skip)
  }
}

