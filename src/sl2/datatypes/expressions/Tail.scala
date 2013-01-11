package sl2.datatypes.expressions

import sl2.datatypes.Expression

case class Tail(e : Expression) extends Expression {

  def isValue(): Boolean = { false }

}