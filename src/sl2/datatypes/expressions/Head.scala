package sl2.datatypes.expressions

import sl2.datatypes._

case class Head(e : Expression) extends Expression {
	override def isValue = false
}