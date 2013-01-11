package sl2.datatypes.expressions

import sl2.datatypes._

case class Op(e1: Expression, op: Operator, e2: Expression) extends Expression{
    override def isValue = false
}