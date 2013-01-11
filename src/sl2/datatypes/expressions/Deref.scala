package sl2.datatypes.expressions

import sl2.datatypes._
import sl2.datatypes.types._

case class Deref(loc: IntRef) extends Expression{
    override def isValue = false
}