package sl2.datatypes.expressions

import sl2.datatypes._

case class N(n: Int) extends Expression{
    override def isValue = true;
}