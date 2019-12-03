package xyz.hyperreal.yola

class Value(val containing: YType, instanceScope: Scope) {
  def instanceOf(typ: YType) = typ == containing || containing.descendantOf(typ)

  def member(name: String) = instanceScope(name)

}

abstract class PrimitiveValue[T](containing: YType, instanceScope: Scope)
    extends Value(containing, instanceScope) {
  val primitive: T

  override def toString = String.valueOf(primitive)
}

object NullValue extends PrimitiveValue[Null](NullType, null) {
  val primitive = null
}
