package sl2.datatypes.expressions

import sl2.datatypes._

case class Var(value: String)  extends Expression{
    override def isValue = true
}