package xyz.hyperreal.yola

class Value(val containing: YType, instanceScope: Scope) {
  def instanceOf(typ: YType) = typ == containing || containing.descendantOf(typ)

  def member(name: String) = instanceScope(name)

}

abstract class WrapperValue[T](containing: YType, instanceScope: Scope)
    extends Value(containing, instanceScope) {
  val wrapped: T

  override def toString = String.valueOf(wrapped)
}
