package sl2.datatypes.expressions

import sl2.datatypes._

case class B(b: scala.Boolean) extends Expression{
    override def isValue = true
}