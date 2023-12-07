package triemaps

import scala.annotation.tailrec

sealed trait Expression
object Expression {
  case class Variable(name: String) extends Expression
  case class Abstraction(parameter: String, body: Expression) extends Expression
  case class Application(function: Expression, argument: Expression) extends Expression

  def lambda(parameter: String, f: Variable => Expression): Expression =
    Abstraction(parameter, f(Variable(parameter)))

  def pretty(expression: Expression): String =
    expression match {
      case Variable(variable)              => variable
      case Abstraction(parameter, body)    => s"λ$parameter.${pretty(body)}"
      case Application(function, argument) => s"${wrap(function)} ${wrap(argument)}"
    }

  private def wrap(expression: Expression): String =
    expression match {
      case variable: Variable       => pretty(variable)
      case abstraction: Abstraction => s"(${pretty(abstraction)})"
      case application: Application => s"(${pretty(application)})"
    }

  def freeVariables(expression: Expression): LazyList[String] = {
    @tailrec
    def nextFreeVariable(
      expressions: List[Expression],
      boundVariables: Set[String]
    ): Option[(String, (List[Expression], Set[String]))] =
      expressions match {
        case Variable(variable) :: tail =>
          if (boundVariables.contains(variable))
            nextFreeVariable(tail, boundVariables)
          else
            Some((variable, (tail, boundVariables)))
        case Abstraction(parameter, body) :: tail =>
          nextFreeVariable(body :: tail, boundVariables.incl(parameter))
        case Application(function, argument) :: tail =>
          nextFreeVariable(function :: argument :: tail, boundVariables)
        case Nil =>
          None
      }

    LazyList.unfold((List(expression), Set.empty[String]))(nextFreeVariable)
  }
}