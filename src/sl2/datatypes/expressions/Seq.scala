package sl2.datatypes.expressions

import sl2.datatypes._

case class Seq(e1: Expression, e2: Expression) extends Expression{
    override def isValue = false
}