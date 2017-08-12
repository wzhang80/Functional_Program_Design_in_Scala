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
    for{
      (name, expression) <- namedExpressions
    } yield (name, Signal(eval(expression(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case ex : Literal => ex.v
      case ex : Ref => eval(getReferenceExpr(ex.name, references), references - ex.name)
      case ex : Plus => eval(ex.a, references) + eval(ex.b, references)
      case ex : Minus => eval(ex.a, references) - eval(ex.b, references)
      case ex : Times => eval(ex.a, references) * eval(ex.b, references)
      case ex : Divide => eval(ex.a, references) / eval(ex.b, references)
      case _ => Double.NaN
    }
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
