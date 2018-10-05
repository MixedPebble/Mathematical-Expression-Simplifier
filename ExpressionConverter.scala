import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap

/*
2014 project
Takes a mathematical expressions and returns the expression in its simplest form.
Simplification works by examining the expression and checking to see if it can be simplified using a known process
When a problem can't be simplified using a known method it is broken down into two halves, left (l) and right (r).
Each half is then checked to see if it can be simplified using a known process.
This happens to each subsequent half until a problem is simplified as much as possible.
*/

abstract class Expression

case class Plus(l: Expression, r: Expression) extends Expression {
	override def toString(): String = "((" + l.toString() + "+" + r.toString() + "))"
}
case class Minus(l: Expression, r: Expression) extends Expression {
	override def toString(): String = "((" + l.toString() + "-" + r.toString() + "))"
}
case class Times(l: Expression, r: Expression) extends Expression {
	override def toString(): String = "(" + l.toString() + "*" + r.toString() + "))"
}
case class Divide(l: Expression, r: Expression) extends Expression {
	override def toString(): String = "((" + l.toString() + "/" + r.toString() + "))"
}
case class Var(n: String) extends Expression {
	override def toString() = n
}
case class Const(v: Double) extends Expression {
	override def toString() = v.toString
}

object Main {
	type BindingList = String => Option[Float]
	
	def printExp(t: Expression) = t match{
		case Plus(l, r) if(l.isInstanceOf[Expression]&&r.isInstanceOf[Expression]) => print(l + "+" + r)
		case Minus(l, r) if(l.isInstanceOf[Expression]&&r.isInstanceOf[Expression]) => print(l + "-" + r)
		case Times(l, r) if(l.isInstanceOf[Expression]&&r.isInstanceOf[Expression]) => print(l + "*" + r)
		case Divide(l, r) if(l.isInstanceOf[Expression]&&r.isInstanceOf[Expression]) => print(l + "/" + r)
		case t => println(t);
	}
	
	def substBinding(e: Expression, bind: BindingList): Expression = e match {
		case Plus(l, r) => Plus(substBinding(l, bind), substBinding(r, bind))
		case Minus(l, r) => Minus(substBinding(l, bind), substBinding(r, bind))
		case Times(l, r) => Times(substBinding(l, bind), substBinding(r, bind))
		case Divide(l, r) => Divide(substBinding(l, bind), substBinding(r, bind))
		case Var(n) =>
			if(bind(n).isDefined) {
				Const(bind(n).get)
			}
			else {
				Var(n)
			}
		case Const(v) => Const(v)
	}
	
	def simplifyTrip(e: Expression): Expression = e match {
		case Plus(Const(l), Const(r)) => Const(l + r);
		case Plus(l, Const(r)) if(r == 0) => simplify(l);
		case Plus(Const(l), r) if(l == 0) => simplify(r);
		case Plus(l, Minus(Const(a), b)) if(a == 0 && l == b) => Const(0);
		case Plus(l, r) if(simplify(l).isInstanceOf[Const] && simplify(r).isInstanceOf[Const]) => simplify(Plus(simplify(l), simplify(r)));
		case Plus(l, r) => Plus(simplify(l), simplify(r));
		case Minus(Const(l), Const(r)) => Const(l-r);
		case Minus(l, Const(r)) if(r == 0) => simplify(l);
		case Minus(l, r) if(l == r) => Const(0);
		case Minus(l, r) if(simplify(l).isInstanceOf[Const] && simplify(r).isInstanceOf[Const]) => simplify(Minus(simplify(l), simplify(r)));
		case Minus(l, r) => Minus(simplify(l), simplify(r));
		case Times(Const(l), Const(r)) => Const(l*r);
		case Times(l, Const(r)) if (r == 0) => Const(0);
		case Times(Const(l), r) if (l == 0) => Const(0);
		case Times(l, Const(r)) if (r == 1) => simplify(l);
		case Times(l, r) if (l == Const(1)) => simplify(r);
		case Times(l, r) if(simplify(l).isInstanceOf[Const]&&simplify(r).isInstanceOf[Const]) => simplify(Times(simplify(l), simplify(r)));
		case Times(l, r) => Times(simplify(l), simplify(r));
		case Divide(Const(l), Const(r)) => Const(l/r);
		case Divide(Const(l), r) if(l == 0) => Const(0);
		case Divide(l, Const(r)) if(r == 1) => simplify(l);
		case Divide(l, r) if(l == r) => Const(1);
		case Divide(l, r) if(simplify(l).isInstanceOf[Const]&&simplify(r).isInstanceOf[Const]) => simplify(Divide(simplify(l), simplify(r)));
		case Divide(l, r) => Divide(simplify(l), simplify(r));
		case _ => e;
	}
	
	def simplify(e: Expression): Expression = e match {
		case Plus(l, r) => simplifyTrip(Plus(simplify(l), simplify(r)))
		case Minus(l, r) => simplifyTrip(Minus(simplify(l), simplify(r)))
		case Times(l, r) => simplifyTrip(Times(simplify(l), simplify(r)))
		case Divide(l, r) => simplifyTrip(Divide(simplify(l), simplify(r)))
		case Var(n) => Var(n)
		case Const(v) => Const(v)
	}
	
    def eval(e: Expression, bind: BindingList): Expression = {
		simplify(substBinding(e, bind))
    }
	
	def main(args: Array[String]) {

		val vars = new HashMap[String, Float]()
		val p1: Expression = Plus(Var("x"), Times(Var("x"), Minus(Var("y"), Divide(Var("z"), Const(2)))))
		val p2: Expression = Plus(Minus(Var("z"), Const(2)), Times(Var("x"), Const(5)))
		val p3: Expression = Plus(Const(1), Var("a"))
		
		println("Expression: " + p1)
		println("Expression: " + p2)
		println("Expression: " + p3)

		println("Bindings: ")
		println("ex: x 0 y 3 z 5")
		println("ex: x 5 z 6")
		
		val s = new java.util.Scanner(scala.io.StdIn.readLine())
		
		while(s.hasNext()) {
			vars.put(s.next(), s.nextFloat())
		}
		
		val bL: BindingList = {case x => vars.get(x)}

		println("Expression: ")
		println("Plus(Var(x), Times(Var(x), Minus(Var(y), Divide(Var(z), Const(2)))))")
		println("Expression With Subs: " + substBinding(p1, bL))
		println("Expression after simplification: ")
		println(simplify(substBinding(p1, bL)))
		printExp(simplify(substBinding(p1, bL)))
		println("---")

		println("Expression: ")
		println("Plus(Minus(Var(z), Const(2)), Times(Var(x), Const(5)))")
		println("Expression With Subs: " + substBinding(p2, bL))
		println("Expression after simplification: ")
		println(simplify(substBinding(p2, bL)))
		printExp(simplify(substBinding(p2, bL)))
		println("---")

		println("Expression: ")
		println("Plus(Const(1), Var(a))")
		println("Expression With Subs: " + substBinding(p3, bL))
		println("Expression after simplification: ")
		println(simplify(substBinding(p3, bL)))
		printExp(simplify(substBinding(p3, bL)))
		println("---")

		//eval(p1, bL))
    }
}
