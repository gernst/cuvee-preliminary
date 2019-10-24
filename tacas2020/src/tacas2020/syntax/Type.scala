package tacas2020.syntax

sealed trait Type

object Type {
  val unit = base("Unit")
  val _int = base("Int")
  val _boolean = base("Boolean")
  
  case class base(ident: Id) extends Type
  def base(name: String): base = base(Id(name))
    
  case class set(elem: Type) extends Type
  case class list(elem: Type) extends Type
  case class array(elem: Type) extends Type
  case class map(dom: Type, ran: Type) extends Type
}