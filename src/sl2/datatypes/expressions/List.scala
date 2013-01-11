package sl2.datatypes.expressions

import sl2.datatypes.Expression

case class List(e1 : Expression, e2 : Expression) extends Expression {

  def isValue(): Boolean = { (e1 isValue) && (e2 isValue) }

}