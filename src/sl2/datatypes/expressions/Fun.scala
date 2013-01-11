package sl2.datatypes.expressions

import sl2.datatypes._
import sl2.datatypes.expressions._

case class Fun(x:Var, t:Type , e:Expression) extends Expression {
    override def isValue = true;
}
