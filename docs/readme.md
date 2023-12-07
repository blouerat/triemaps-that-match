# Triemaps that match

```scala mdoc
import triemaps._
import Expression._

val I = lambda("x", identity)
val map = ExpressionMap.singleton(AlphaExpression.closed(I), 42)
println(map.getClosedExpression(I))
println(map.getClosedExpression(lambda("y", identity)))
```

```haskell
{−# RULES "map/map" ∀f g xs. map f (map g xs) = map (f ◦ g) xs #−}
```

```scala mdoc
import triemaps.Matching._
val lhs =
  Application(
    Application(Variable("map"), Variable("f")),
    Application(
      Application(Variable("map"), Variable("g")),
      Variable("xs")
    )
  )
val pattern = ExpressionPattern(Map("f" -> 0, "g" -> 1, "xs" -> 2), AlphaExpression.closed(lhs))
val rhs =
  Application(
    Application(
      Variable("map"),
      Application(
        Application(Variable("compose"), Variable("f")),
        Variable("g")
      )
    ),
    Variable("xs")
  )

matchExpression(pattern, AlphaExpression.closed(lhs)).run(Map.empty)
```
