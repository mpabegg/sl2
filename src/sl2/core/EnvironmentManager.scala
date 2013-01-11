package sl2.core;

import sl2.datatypes.expressions._
import sl2.datatypes._
import sl2.datatypes.types._
import scala.collection.mutable._

object EnvironmentManager {

    type Sigma = HashMap[IntRef, N];
    type Gamma = HashMap[Var, Type];

    def sigma = new Sigma;    
    def gamma = new Gamma;
}
