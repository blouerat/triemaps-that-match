package triemaps

object Matching {
  case class ExpressionPattern(patternKeys: Map[String, Int], alphaExpression: AlphaExpression) {
    def withExpression(newExpression: Expression): ExpressionPattern = copy(alphaExpression = alphaExpression.copy(expression = newExpression))
  }
  case class MatchExpr[A](run: Map[Int, Expression] => List[(Map[Int, Expression], A)]) {
    def map[B](f: A => B): MatchExpr[B] = MatchExpr(subst => run(subst).map {
      case (newSubst, a) => (newSubst, f(a))
    })
    def flatMap[B](f: A => MatchExpr[B]): MatchExpr[B] = MatchExpr { subst =>
      run(subst).flatMap {
        case (newSubst, a) => f(a).run(newSubst) // TODO: is this correct?
      }
    }
  }
  object MatchExpr {
    def empty[A]: MatchExpr[A] = MatchExpr(_ => Nil)
    def pure[A](value: A): MatchExpr[A] = MatchExpr(subst => List((subst, value)))
  }

  sealed trait CanonicalPatternVariable
  case class Free(variable: String) extends CanonicalPatternVariable
  case class Bound(index: Int) extends CanonicalPatternVariable
  case class Pat(index: Int) extends CanonicalPatternVariable
  object CanonicalPatternVariable {
    def apply(patternKeys: Map[String, Int], deBruijnState: DeBruijnState, variable: String): CanonicalPatternVariable =
      deBruijnState.get(variable).map(Bound.apply)
        .orElse(patternKeys.get(variable).map(Pat.apply))
        .getOrElse(Free(variable))
  }

  def matchExpression(pattern: ExpressionPattern, target: AlphaExpression): MatchExpr[Unit] =
    pattern.alphaExpression.expression match {
      case Expression.Variable(variable)              =>
        CanonicalPatternVariable(pattern.patternKeys, pattern.alphaExpression.deBruijnState, variable) match {
          case Pat(index)     => matchPatVarE(index, target)
          case occ => target.expression match {
            case Expression.Variable(targetVariable) if CanonicalPatternVariable(Map.empty, target.deBruijnState, targetVariable) == occ => MatchExpr.pure(())
            case _ => MatchExpr.empty
          }
        }
      case Expression.Abstraction(parameter, body)    =>
        target.expression match {
          case Expression.Abstraction(parameter2, body2) =>
            matchExpression(
              pattern = pattern.copy(alphaExpression = AlphaExpression(pattern.alphaExpression.deBruijnState.add(parameter), body)),
              target = AlphaExpression(target.deBruijnState.add(parameter2), body2)
            )
          case _ => MatchExpr.empty
        }
      case Expression.Application(function, argument) =>
        target.expression match {
          case Expression.Application(function2, argument2) =>
            for {
              _ <- matchExpression(pattern.withExpression(function), target.copy(expression = function2))
              result <- matchExpression(pattern.withExpression(argument), target.copy(expression = argument2))
            } yield result
          case _ => MatchExpr.empty
        }
    }

  def matchPatVarE(patternKey: Int, alphaExpression: AlphaExpression): MatchExpr[Unit] =
    refineMatch { subst =>
      subst.get(patternKey) match {
        case Some(sol) if equalsClosedExpression(alphaExpression.expression, sol) && noCaptured(alphaExpression) => Some(subst)
        case None if noCaptured(alphaExpression) => Some(subst.updated(patternKey, alphaExpression.expression))
        case _ => None
      }
    }

  def equalsClosedExpression(e1: Expression, e2: Expression): Boolean =
    equalsModAlphaExpression(AlphaExpression.closed(e1), AlphaExpression.closed(e2))

  def equalsModAlphaExpression(e1: AlphaExpression, e2: AlphaExpression): Boolean =
    (e1.expression, e2.expression) match {
      case (Expression.Variable(variable1), Expression.Variable(variable2)) =>
        (e1.deBruijnState.get(variable1), e2.deBruijnState.get(variable2)) match {
          case (Some(boundVariable1), Some(boundVariable2)) => boundVariable1 == boundVariable2
          case (None, None) => variable1 == variable2
          case _ => false
        }
      case (Expression.Abstraction(parameter1, body1), Expression.Abstraction(parameter2, body2)) =>
        equalsModAlphaExpression(
          AlphaExpression(e1.deBruijnState.add(parameter1), body1),
          AlphaExpression(e2.deBruijnState.add(parameter2), body2)
        )
      case (Expression.Application(function1, argument1), Expression.Application(function2, argument2)) =>
        equalsModAlphaExpression(e1.copy(expression = function1), e2.copy(expression = function2)) &&
          equalsModAlphaExpression(e1.copy(expression = argument1), e2.copy(expression = argument2))
      case _ => false
    }

  def noCaptured(alphaExpression: AlphaExpression): Boolean =
    !Expression.freeVariables(alphaExpression.expression).exists(alphaExpression.deBruijnState.contains)

  def refineMatch(f: Map[Int,Expression] => Option[Map[Int,Expression]]): MatchExpr[Unit] =
    MatchExpr { subst =>
    f(subst) match {
        case Some(newSubst) => List((newSubst, ()))
        case None        => Nil
      }
    }
}
