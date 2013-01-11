package sl2.datatypes.expressions

import sl2.datatypes._
import sl2.datatypes.types._

case class LetRec(f: Var, tf: Function, fn: Fun, in: Expression) extends Expression{
    override def isValue = false
}