package sl2

import org.junit._
import Assert._

import sl2.core._
import sl2.datatypes._
import sl2.datatypes.types._
import sl2.datatypes.expressions._
import sl2.datatypes.operators._

class Tests {

    var gamma = new EnvironmentManager.Gamma
    var sigma = new EnvironmentManager.Sigma

    @Before
    def setUp(){
	sigma clear
    }

    /**
     * Returns a LetRec expression, with the definition of Factorial of N
     * @param n
     * 	The number of which the factorial should be calculated
     * @return
     * An Expression equivalent to :
     * <br/>
     * <code>
     * let rec fat : int -> int = <br/>
     * &nbsp;&nbsp;&nbsp;&nbsp;(fn y : int => if y = 0 then 1 else y * fat ( y-1 ))<br/>
     * &nbsp;&nbsp;in fat N<br/>
     * end
     * </code>
     */
    private def getLetRecWithFactorialOfN(n : Int) : Expression = {

	    def f = Var("fat")
	    def y = Var("y")
	    def in = App(f, N(n))
	    def yGteq1 = Op(y, GE(), N(1))
	    def yMinus1 = Op(y, Plus(), N(-1))
	    def yFatYMinus1 = App(f, yMinus1)
	    def thenExpr = Op(y, Times(), yFatYMinus1)
	    def elseExpr = N(1) 
	    def ifExpr = If(yGteq1, thenExpr, elseExpr)
	    def fn = Fun(y, Integer(), ifExpr)

	    LetRec(f, Function(Integer(), Integer()), fn, in)
    }

    private def getLIst1to5PlusNil = {
	    List(N(1), List(N(2), List(N(3), List(N(4), List(N(5), Nil())))))
    }

    private def getList1To2 = {
	    List(N(1),N(2))
    }

    @Test
    def letRecFactorialOf5ShouldReturn120 {

	val expected = N(120)

	val expr = getLetRecWithFactorialOfN(5)

	val result = Processor.evaluate(expr, sigma)

	println("Fatorial de 5 = " + result._1)

	assertEquals(expected, result _1)

	println
    }

    @Test
    def letRecFactorialOf10ShouldReturn3628800 {

	val expected = N(3628800)

	val expr = getLetRecWithFactorialOfN(10)

	val result = Processor.evaluate(expr, sigma)

	println("Fatorial de 10 = " + result._1)

	assertEquals(expected, result _1)

	println
    }


    @Test
    def letRecFactorialOf5ShouldReturnInteger{
	val expected = Integer()
	val result = Processor.inferType(getLetRecWithFactorialOfN(5), gamma, sigma)

	println("Tipo de Fatorial de 5 = " + result.get)

	assertEquals(expected, result get)

	println
    }

    @Test
    def headShouldReturnFirstElementOfTheList{
	var list = getList1To2
	var result = Processor.evaluate(Head(list), sigma)

	println("Head de " + list + " = " + result._1)

	assertEquals(list.e1, result _1)

	list = getLIst1to5PlusNil
	result = Processor.evaluate(Head(list), sigma)

	println("Head de " + list + " = " + result._1)

	assertEquals(list.e1, result _1)

	println
    }

    @Test
    def tailShouldReturnLastElementOfTheLis{
	var list = getList1To2
	var result = Processor.evaluate(Tail(list), sigma)

	println("Tail de " + list + " = " + result._1)

	assertEquals(list.e2, result _1)

	list = getLIst1to5PlusNil
	result = Processor.evaluate(Tail(list), sigma)

	println("Tail de " + list + " = " + result._1)

	assertEquals(list.e2, result _1)
	println
    }

    @Test
    def exercise11ShouldReturnNotEvaluateTillTheEnd{
	var expr = Op(Op(N(2), Plus(), N(3)), Plus(), Op(N(3), GE(), B(true)))

	var actualValue = Processor.evaluate(expr, sigma)
	var actualType = Processor.inferType(expr, gamma, sigma)
	
	
	assertEquals(Op(N(5),Plus(),Op(N(3),GE(),B(true))), actualValue _1)
	assertEquals(None, actualType)
    }
    
    private def setSigmaForexercise12{
	sigma += ((IntRef("l0"), N(0)))
	sigma += ((IntRef("l1"), N(0)))
    }
    
    @Test
    def exercie12ShouldReturnSkipAndTheCorrectMemory{
	setSigmaForexercise12
	
	var expr = Seq(Assign(IntRef("l0"), N(7)), Assign(IntRef("l1"), Op(Deref(IntRef("l0")), Plus(), N(2))))
	
	var actualValue = Processor.evaluate(expr, sigma)
	var actualType = Processor.inferType(expr, gamma, sigma)
	
	assertEquals(Skip(), actualValue _1)
	assertEquals(MyUnit(), actualType get)
	
	assertEquals(Map(IntRef("l0") -> N(7), IntRef("l1") -> N(9)), actualValue _2)
    }
    
    private def getExpressionForEx17 = {
	Let(
		Var("x"), 
		Function(Integer(), Integer()), 
		Fun(Var("y"), Integer(), Op(Var("y"), Plus(), N(10))),
		App(Var("x"), N(10)))
    }
    
    @Test
    def exercise17ShouldReturn20AndTypeInt{
	var expr = getExpressionForEx17
	var actualValue = Processor.evaluate(expr, sigma)
	var actualType = Processor.inferType(expr, gamma, sigma)
	
	assertEquals(N(20), actualValue _1)
	assertEquals(Integer(), actualType get)
    }
    
    
}