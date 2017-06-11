
trait Expr {

}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Var(v: String) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

object ExprTest{
  def eval(e: Expr ): Int = e match {
    case Number(n) =>  n
    case Sum(e1,e2) => eval(e1) + eval (e2)
    case Prod(e1,e2) => eval(e1) * eval(e2)
  }

  def show(e: Expr ): String = e match {
    case Number(n) =>  n.toString
    case Sum(e1,e2) => show(e1).toString + " + " +  show (e2).toString
    case Prod(Sum(e1,e2),e3) => "( " + show(Sum(e1,e2)).toString() + " ) * " + show(e3).toString()
    case Prod(e1,Sum(e2,e3)) => show(e3).toString() + " * ( " + show(Sum(e2,e3)).toString() + " ) "
    case Prod(Sum(e1,e2),Sum(e3,e4)) => "( " + show(Sum(e1,e2)).toString() + " ) * ( " + show(Sum(e3,e4)).toString() + " ) "
    case Prod(e1,e2) => show(e1).toString + " * " + show(e2).toString()
    case Var(v) => v
  }
}

ExprTest.show(new Sum(new Number(1), new Prod(new Sum(new Number(9), new Var("b")), new Var("a"))))