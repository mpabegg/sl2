package sl2.datatypes.expressions

import sl2.datatypes._

case class Let(x: Var, t: Type, e1 : Expression, e2: Expression) extends Expression{
    override def isValue = false
}