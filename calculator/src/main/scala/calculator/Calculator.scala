package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
      for (e <- namedExpressions)
        yield (e._1,Signal(eval(e._2(),namedExpressions-e._1)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    //val refs = references.mapValues(_ => _())
    def evalRec(expr: Expr, forb: Set[String]) : Double = expr match {
      case Literal(v) => v
      case Ref(n) => if (!forb.contains(n)) evalRec(getReferenceExpr(n,references),forb + n) else Double.NaN
      case Plus(a,b) => evalRec(a,forb) + evalRec(b,forb)
      case Minus(a,b) => evalRec(a,forb) - evalRec(b,forb)
      case Times(a,b) => evalRec(a,forb) * evalRec(b,forb)
      case Divide(a,b) => evalRec(a,forb) / evalRec(b,forb)
    }

    evalRec(expr,Set())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
