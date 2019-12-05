package xyz.hyperreal.yola

class Value(val containing: YType, instanceScope: Scope) {
  def isInstanceOf(typ: YType) = typ == containing || containing.descendantOf(typ)

  def asInstanceOf(typ: YType) =
    if (isInstanceOf(typ)) this else sys.error(s"not an instance of $typ: $this")

  def member(name: String) = instanceScope(name)

  // todo: iterator method is temporary
  def iterator: Iterator[Value] = ???

}

abstract class WrappedValue[T](containing: YType, instanceScope: Scope)
    extends Value(containing, instanceScope) {
  val v: T

  override def toString = String.valueOf(v)
}
