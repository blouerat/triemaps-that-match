package triemaps

import Expression._

@main
def main(): Unit = {
  val I = lambda("x", identity)
  val map = ExpressionMap.singleton(AlphaExpression.closed(I), 42)
  println(map.getClosedExpression(I))
  println(map.getClosedExpression(lambda("y", identity)))
}