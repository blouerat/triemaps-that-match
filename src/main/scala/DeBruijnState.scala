package triemaps

case class DeBruijnState(next: Int, bindings: Map[String, Int]) {
  def add(variable: String): DeBruijnState = DeBruijnState(next = next + 1, bindings = bindings.updated(variable, next))
  def get(variable: String): Option[Int] = bindings.get(variable)
  def contains(variable: String): Boolean = bindings.contains(variable)
}
object DeBruijnState {
  def empty: DeBruijnState = DeBruijnState(next = 1, bindings = Map.empty)
}
