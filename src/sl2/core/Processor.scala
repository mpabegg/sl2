package sl2.core;
import sl2.datatypes._
import sl2.datatypes.expressions._
import sl2.datatypes.types._
import sl2.datatypes.operators._
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet

object Processor {

    /**
     * Takes one step on an Expression reduction
     */
    private def reduce(e: Expression, sigma: EnvironmentManager.Sigma): Option[(Expression,  EnvironmentManager.Sigma)] = { 
	    e match {
		// N
		case N(_) => None
		// B
		case B(_) => None
		// Skip
		case Skip() =>  None
		// Op
		case operation:Op =>{ 
		    operation match {
			// Op +
			case Op(n1: N, op1: Plus, n2: N) => Some((N(n1.n + n2.n), sigma))
			// Op >=
			case Op(n1: N, op1: GE, n2: N) => Some((B(n1.n >= n2.n), sigma))
			// Op *
			case Op(n1 : N, op1 : Times, n2 : N) => Some((N(n1.n * n2.n), sigma))

			case Op(e1, op, e2) => {
			    // Op 2
			    if (e1 isValue){
				reduce(e2,sigma) match{
				    case Some((e2_, sigma_)) => Some(Op(e1, op, e2_), sigma_)
				    case None => None
				}
			    }
			    else{
				// Op 1
				reduce(e1, sigma) match{
				    case Some((e1_, sigma_)) => Some(Op(e1_, op, e2), sigma_)
				    case None => None
				}
			    }
			}
		    }
		}
		// If
		case If(cond, e1, e2) => {
		    cond match{
			//If 1
			case B(true) => Some(e1, sigma)
			// If 2
			case B(false) => Some(e2, sigma)
			case _ => {
			    reduce(cond, sigma) match {
				// If 3
				case Some((cond_, sigma_)) => Some(If(cond_, e1, e2), sigma_)
				case None => None
			    }
			}
		    }
		}
		// Seq
		case Seq(e1, e2) => {
		    e1 match{
			// Seq 1
			case Skip() => Some(e2, sigma)
			// Seq 2
			case _ => {
			    reduce(e1, sigma) match {
				case Some((e1_, sigma_)) => Some(Seq(e1_, e2), sigma)
				case None => None
			    }
			}
		    }
		}
		// ATR
		case Assign(loc, e) => {
		    e match {
			// ATR 1
			case n:N => {
			    if(sigma.contains(loc)){
				sigma(loc) = n
				Some(Skip(), sigma)
			    } else None
			} 
			// ATR 2
			case _ => {
			    reduce(e,sigma) match {
				case Some((e_, sigma_)) => Some((Assign(loc, e_), sigma_))
				case _ => None
			    }
			}
		    } 
		}
		// DEREF
		case Deref(loc) => Some(sigma(loc), sigma)
		// While
		case While(e1, e2) => Some(If(e1, Seq(e2, While(e1, e2)), Skip()), sigma) 
		// FUN
		case Fun(Var(x), t, e) => None
		//BETA
		case App(Fun(x, t, e1), e2) => Some(replace(e2, x, e1) get, sigma)
		// APP
		case App(e1, e2) => {

		    // APP 1
		    if (e1 isValue){
			reduce (e2, sigma) match {
			    case Some((e2_, sigma_)) => Some((App(e1, e2_), sigma_))
			    case None => None
			}
			// APP 2
		    } else {
			reduce (e1, sigma) match {
			    case Some((e1_, sigma_)) => Some((App(e1_, e2), sigma_))
			    case None => None
			}
		    }
		}
		// LET
		case Let(x, t, e1, e2) => {
		    // LET 1
		    if (e1 isValue){
			Some(replace(e1, x, e2) get, sigma)
			// LET 2
		    } else {
			reduce (e1, sigma) match {
			    case Some((e1_, sigma_)) => Some((Let(x, t, e1_, e2), sigma_))
			    case None => None
			}
		    }
		}
		// LET REC
		case LetRec(f, tf, fn, in) => {
		    Some((replace(Fun(fn.x, tf.t1,LetRec(f, tf, fn, fn.e)), f, in) get, sigma))
		}
		// LIST
		case List(e1, e2) => {
		    if (e.isValue)
			None
			else{
			    if(e1 isValue){
				reduce(e2, sigma) match {
				    case Some((e2_, sigma_)) => Some(List(e1, e2_), sigma_)
				    case _ => None
				}
			    } else{
				reduce (e1, sigma) match {						
				    case Some((e1_, sigma_)) => Some(List(e1_, e2), sigma_)
				    case _ => None
				}
			    }
			}
		}
		// NIL
		case Nil() => None
		// HEAD
		case Head(e) => {				
		    reduce(e, sigma) match {
			case Some((e1_, sigma_)) => Some(Head(e1_), sigma_)
			case _ => {
			    e match{
				case List(v1, v2) => {	
				    if(e isValue) Some((v1, sigma))
				    else None
				}
				case _ => None
			    }
			}
		    }
		}			
		// TAIL
		case Tail(e) => {				
		    reduce(e, sigma) match {
			case Some((e1_, sigma_)) => Some(Tail(e1_), sigma_)
			case _ => {
			    e match{					
				case List(v1, v2) => {	
				    if(e isValue) Some((v2, sigma))
				    else None
				}
				case _ => None
			    }
			}
		    }
		}
	    } // FIM DO MATCH e
    }


    /**
     * Evaluates an Expression
     */
    def evaluate(e1: Expression, s:EnvironmentManager.Sigma ) : (Expression, EnvironmentManager.Sigma) = {
	reduce(e1, s) match {
	    case None => (e1, s)
	    case Some((e1_, s_)) => evaluate(e1_, s_)
	}
    }

    /**
     * Infers the type of a given Expression, given the Types Environment (gamma) and the "Variables" Environment (sigma)
     */
    def inferType(e: Expression, gamma:EnvironmentManager.Gamma, sigma: EnvironmentManager.Sigma) : Option[Type] = e match {
	// N
	case N(_) => Some(Integer())
	// B
	case B(_) => Some(Boolean())
	// Skip
	case Skip() =>  Some(MyUnit())
	// Op
	case operation:Op => operation match {
	    // Op +
	    case Op(n1, op1: Plus, n2) => 
	    (inferType(n1, gamma, sigma), inferType(n2, gamma, sigma)) match {
		case (Some(Integer()), Some(Integer())) => Some(Integer())
		case _ => None
	    }
	    // Op *
	    case Op(n1, op1: Times, n2) => 
	    (inferType(n1, gamma, sigma), inferType(n2, gamma, sigma)) match {
		case (Some(Integer()), Some(Integer())) => Some(Integer())
		case _ => None
	    }
	    // Op >=
	    case Op(n1, op1: GE, n2) => 
	    (inferType(n1, gamma, sigma), inferType(n2, gamma, sigma)) match { 
		case (Some(Integer()), Some(Integer())) => Some(Boolean())
		case _ => None
	    }
	    case _ => None
	}
	// If
	case If(cond, e1, e2) => {
	    (inferType(cond, gamma, sigma),inferType(e1, gamma, sigma), inferType(e2, gamma, sigma)) match {
		case (Some(Boolean()), Some(t1), Some(t2)) => {
		    if (t1 equals t2)
			Some(t1);
		    else None
		}
		case _ => None
	    }
	}
	// Seq
	case Seq(e1, e2) => {
	    (inferType(e1, gamma, sigma), inferType(e2, gamma, sigma)) match { 
		case (Some(MyUnit()), Some(t)) => Some(t)
		case _ => None
	    }
	}
	// ATR
	case Assign(loc, n) => {
	    (inferType(n, gamma, sigma), sigma.keySet(loc)) match {
		case (Some(Integer()), true) => Some(MyUnit())
		case _ => None
	    }
	}  
	// DEREF
	case Deref(loc) => {
	    if (sigma.keySet(loc))
		Some(Integer());
	    else None
	}
	// While
	case While(e1, e2) => {
	    (inferType(e1, gamma, sigma), inferType(e2, gamma, sigma)) match {
		case (Some(Boolean()), Some(MyUnit())) => Some(MyUnit())
		case _=> None
	    }
	} 
	// FUN
	case Fun(x, t, e) => {
	    gamma += ((x, t))
	    inferType(e, gamma, sigma) match {
		case Some(t_) => Some(Function(t, t_))
		case _ => None
	    }
	}
	// APP
	case App(e1, e2) =>{
	    (inferType(e1, gamma, sigma), inferType(e2, gamma, sigma)) match {
		case (Some(Function(t1, t_)), Some(t2)) => {
		    if (t1 equals t2)
			Some(t_);
		    else None
		}
		case _ => None
	    }
	}
	// LET
	case Let(x, t, e1, e2) =>{
	    (inferType(e1, gamma, sigma), inferType(e2, (gamma += ((x, t))), sigma)) match {
		case (Some(t), Some(t_)) => Some(t_)
		case _ => None
	    }
	}
	// LETREC
	case LetRec(f, tf, fn, in) => {
	    (inferType(fn.e, (gamma += ((f, tf)) += ((fn.x, tf.t1))), sigma), inferType(in, (gamma +=((f, tf))), sigma)) match {
		case (Some(tf.t2), Some(t)) => Some(t)
		case _ => None
	    }
	}
	case Var(x) => {
	    Some(gamma(Var(x)))
	}
    }

    /**
     * Replaces a Var for an Expression in another Expression
     */
    private def replace(e: Expression, x:Var, in: Expression) : Option[Expression] = {
	    in match {

		case Var(x.value) => Some(e)
		case Var(y) => {
		    if (x.value != y)
			Some(in);
		    else None
		}
		case Fun(y, t, e_) => {
		    if ((x.value != y.value) && !getFreeVariables(e).contains(y)){
			Some(Fun(y, t, replace(e, x, e_) get))
		    } else None
		}
		case App(e1, e2) => Some(App(replace(e, x, e1) get, replace(e, x, e2) get))
		case N(n) => Some(in)
		case Op(e1, op, e2) => Some(Op(replace(e, x, e1) get, op, replace(e, x, e2) get))
		case If(cond, e1, e2) => Some(If(replace(e, x, cond) get, replace(e, x, e1) get, replace(e, x, e2) get))
		case B(b) => Some(in)
		case Skip() => Some(in)
		case Assign(l, e1) => Some(Assign(l, replace(e, x, e1) get))
		case Deref(l) => Some(in)
		case Seq(e1, e2) => Some(Seq(replace(e, x, e1) get, replace(e, x, e2) get))
		case While(cond, e1) => Some(While(replace(e, x, cond) get, replace(e, x, e1) get))
		case Let(y, t, e1, e2) => { 
		    if ((x.value != y.value) && !getFreeVariables(e).contains(y))
			Some(Let(y, t, replace(e, x, e1) get, replace(e, x, e2) get));
		    else None
		}
		case LetRec(f, tf, fn, in) => {
		    if (x.value != f.value && !getFreeVariables(e).contains(f)){
			Some(LetRec(f, tf, fn, replace(e, x, in) get))
		    }
		    else None
		}
	    }
    }

    /**
     * Renames the var X, on the expression. 
     * The new name is given by the VariablesManager object
     * 
     * Not yet implemented
     */
    @Deprecated
    private def rename(e: Expression, x: Var) : Option[Expression] = {
	    e match {
		case N(_) => Some(e)
		case B(_) => Some(e)
		case Op(e1, op, e2) => Some(Op(rename(e1, x) get, op, rename(e2, x) get))
		case If(cond, e1, e2) => Some(If(rename(cond, x) get, rename(e1, x) get, rename(e2, x) get))
		case Assign(l, e) => Some(Assign(l, rename(e, x) get))
		case Deref(_) => Some(e)
		case Skip() => Some(e)
		case Seq(e1, e2) => Some(Seq(rename(e1, x) get, rename(e2, x) get))
		case While(cond, e1) => Some(While(rename(cond, x) get, rename(e1, x) get))
		case Fun(x, t, e) => None
		case App(e1, e2) => None
		case Var(_) => None
		case Let(x, t, e1, e2) => None
		case LetRec(f, tf, fn, in) => None
	    }
    }


    /**
     * Gets the free variables on the expression.
     */
    private def getFreeVariables(e : Expression) : Set[Var] = {
	    e match {
		case Var(x) => new HashSet[Var] + Var(x)
		case Fun(y, t, e) => getFreeVariables(e) - y
		case Seq(e1, e2) => getFreeVariables(e1) ++ getFreeVariables(e2)
		case N(_) => new HashSet[Var]
		case Op(e1, _, e2) => getFreeVariables(e1) ++ getFreeVariables(e2)
		case App(e1, e2) => getFreeVariables(e1) ++ getFreeVariables(e2)
		case Assign(loc, e) => getFreeVariables(e)
		case B(_) => new HashSet[Var]
		case Deref(_) => new HashSet[Var]
		case If(cond, e1, e2) => getFreeVariables(cond) ++ getFreeVariables(e1) ++ getFreeVariables(e2)
		case Let(x, t, e1, e2) => getFreeVariables(e1) ++ getFreeVariables(e2) - x
		case LetRec(f, tf, fn, in) => getFreeVariables(fn) ++ getFreeVariables(in)
		case Skip() => new HashSet[Var]
		case While(cond, e) => getFreeVariables(cond) ++ getFreeVariables(e)
		case _ => null
	    }
    }
}