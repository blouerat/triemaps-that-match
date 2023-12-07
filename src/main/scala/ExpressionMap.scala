package triemaps

import Expression._

import scala.collection.mutable

sealed trait ExpressionMap[+A] {
  def get(expression: AlphaExpression): Option[A]
  def updatedWith[AA >: A](expression: AlphaExpression)(remappingFunction: Option[A] => Option[AA]): ExpressionMap[AA]
  def unionWith[AA >: A](other: ExpressionMap[AA])(f: (A, AA) => AA): ExpressionMap[AA]
  def foldRight[B](z: B)(op: (A, B) => B): B
  def isEmpty: Boolean

  final def getClosedExpression(expression: Expression): Option[A] =
    get(AlphaExpression.closed(expression))

  final def updated[AA >: A](expression: AlphaExpression, value: AA): ExpressionMap[AA] =
    updatedWith(expression)(_ => Some(value))

  final def removed(expression: AlphaExpression): ExpressionMap[A] =
    updatedWith(expression)(_ => None)

  final def size: Int =
    foldRight(0) {
      (_, n) => n + 1
    }
}

object ExpressionMap {

  object EmptyExpressionMap extends ExpressionMap[Nothing] {
    override def get(expression: AlphaExpression): Option[Nothing] = None

    override def updatedWith[AA >: Nothing](expression: AlphaExpression)(remappingFunction: Option[Nothing] => Option[AA]): ExpressionMap[AA] =
      remappingFunction(None) match {
        case None        => empty
        case Some(value) => singleton(expression, value)
      }

    override def isEmpty: Boolean = true

    override def unionWith[AA >: Nothing](other: ExpressionMap[AA])(f: (Nothing, AA) => AA): ExpressionMap[AA] = other

    override def foldRight[B](z: B)(op: (Nothing, B) => B): B = z
  }

  final case class NonEmptyExpressionMap[+A](
    freeVariables: Map[String, A],
    boundVariables: Map[Int, A],
    abstractions: ExpressionMap[A],
    applications: ExpressionMap[ExpressionMap[A]]
  ) extends ExpressionMap[A] {

    override def get(alphaExpression: AlphaExpression): Option[A] =
      alphaExpression.expression match {
        case Variable(variable) =>
          alphaExpression.deBruijnState.get(variable) match {
            case Some(index) => boundVariables.get(index)
            case None => freeVariables.get(variable)
          }

        case Abstraction(parameter, body) =>
          abstractions.get(AlphaExpression(alphaExpression.deBruijnState.add(parameter), body))

        case Application(function, argument) =>
          applications
            .get(AlphaExpression(alphaExpression.deBruijnState, function))
            .flatMap(_.get(AlphaExpression(alphaExpression.deBruijnState, argument)))
      }

    override def updatedWith[AA >: A](alphaExpression: AlphaExpression)(remappingFunction: Option[A] => Option[AA]): ExpressionMap[AA] =
      alphaExpression.expression match {
        case Variable(variable) =>
          alphaExpression.deBruijnState.get(variable) match {
            case Some(index) => build(freeVariables, boundVariables.updatedWith(index)(remappingFunction), abstractions, applications)
            case None => build(freeVariables = freeVariables.updatedWith(variable)(remappingFunction), boundVariables, abstractions, applications)
          }
        case Abstraction(parameter, body) =>
          build(freeVariables, boundVariables, abstractions.updatedWith(AlphaExpression(alphaExpression.deBruijnState.add(parameter), body))(remappingFunction), applications)
        case Application(function, argument) =>
          build(freeVariables, boundVariables, abstractions, applications.updatedWith(AlphaExpression(alphaExpression.deBruijnState, function))(liftRemappingFunction(_.updatedWith(AlphaExpression(alphaExpression.deBruijnState, argument))(remappingFunction))))
      }

    override def isEmpty: Boolean = false

    override def unionWith[AA >: A](other: ExpressionMap[AA])(f: (A, AA) => AA): ExpressionMap[AA] =
      other match {
        case EmptyExpressionMap => this
        case nonEmpty: NonEmptyExpressionMap[AA] =>
          NonEmptyExpressionMap(
            freeVariables = mapUnionWith(freeVariables, nonEmpty.freeVariables)(f),
            boundVariables = mapUnionWith(boundVariables, nonEmpty.boundVariables)(f),
            abstractions = abstractions.unionWith(nonEmpty.abstractions)(f),
            applications = applications.unionWith(nonEmpty.applications)(_.unionWith(_)(f))
          )
      }

    override def foldRight[B](z: B)(op: (A, B) => B): B = {
      val z1 = applications.foldRight(z) {
        (em, acc) => em.foldRight(acc)(op)
      }
      val z2 = abstractions.foldRight(z1)(op)
      val z3 = boundVariables.values.foldRight(z2)(op)
      freeVariables.values.foldRight(z3)(op)
    }
  }

  def empty[A]: ExpressionMap[A] = EmptyExpressionMap

  def singleton[A](alphaExpression: AlphaExpression, value: A): ExpressionMap[A] =
    alphaExpression.expression match {
      case Variable(variable) =>
        alphaExpression.deBruijnState.get(variable) match {
          case Some(index) =>
            NonEmptyExpressionMap(freeVariables = Map.empty, boundVariables = Map(index -> value), abstractions = empty, applications = empty)
          case None        =>
            NonEmptyExpressionMap(freeVariables = Map(variable -> value), boundVariables = Map.empty, abstractions = empty, applications = empty)
        }

      case Abstraction(parameter, body) =>
        NonEmptyExpressionMap(
          freeVariables = Map.empty,
          Map.empty,
          abstractions = singleton(AlphaExpression(alphaExpression.deBruijnState.add(parameter), body), value),
          applications = empty
        )

      case Application(function, argument) =>
        NonEmptyExpressionMap(
          freeVariables = Map.empty,
          Map.empty,
          abstractions = empty,
          applications = singleton(AlphaExpression(alphaExpression.deBruijnState, function), singleton(AlphaExpression(alphaExpression.deBruijnState, argument), value))
        )
    }

  private def build[A](
    freeVariables: Map[String, A],
    boundVariables: Map[Int, A],
    abstractions: ExpressionMap[A],
    applications: ExpressionMap[ExpressionMap[A]]
  ): ExpressionMap[A] =
    if (freeVariables.isEmpty && boundVariables.isEmpty && abstractions.isEmpty && applications.isEmpty)
      empty
    else
      NonEmptyExpressionMap(freeVariables, boundVariables, abstractions, applications)

  private def liftRemappingFunction[A, B](f: ExpressionMap[A] => ExpressionMap[B]): Option[ExpressionMap[A]] => Option[ExpressionMap[B]] =
    em => Option(f(em.getOrElse(empty))).filterNot(_.isEmpty)

  private def mapUnionWith[K, V, VV >: V](map: Map[K, V], otherMap: Map[K,VV])(f: (V, VV) => VV): Map[K, VV] = {
    val result = mutable.Map.from[K, VV](map)
    otherMap.foreach {
      (key, value) =>
        result.updateWith(key) {
          case None                => Some(value)
          case Some(existingValue) => Some(f(existingValue.asInstanceOf[V], value))
        }
    }
    result.toMap
  }
}
