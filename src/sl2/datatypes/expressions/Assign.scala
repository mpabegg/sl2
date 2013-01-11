package sl2.datatypes.expressions

import sl2.core._
import sl2.datatypes._
import sl2.datatypes.types._

case class Assign(loc: IntRef, e: Expression) extends Expression{
    override def isValue = false
}