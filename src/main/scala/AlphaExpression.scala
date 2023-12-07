package triemaps

case class AlphaExpression(deBruijnState: DeBruijnState, expression: Expression)
object AlphaExpression {
  def closed(expression: Expression): AlphaExpression = AlphaExpression(DeBruijnState.empty, expression)
}
