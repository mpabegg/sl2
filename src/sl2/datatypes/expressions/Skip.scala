package sl2.datatypes.expressions

import sl2.datatypes._

case class Skip() extends Expression{
    override def isValue = true;
}