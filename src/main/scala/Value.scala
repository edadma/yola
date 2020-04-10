package xyz.hyperreal.yola

import scala.util.parsing.input.Position

class Value(val containing: YType, instanceScope: Scope) {
  def isInstanceOf(typ: YType) = typ == containing || containing.descendantOf(typ)

  def asInstanceOf(typ: YType) =
    if (isInstanceOf(typ)) this else sys.error(s"not an instance of $typ: $this")

  def member(pos: Position, name: String) =
    instanceScope getValue name getOrElse problem(pos, s"member not found: $name")

  // todo: iterator method is temporary
  def iterator: Iterator[Value] = ???

  override def toString = s"${containing.name}@${Integer.toHexString(hashCode)}"

}

abstract class WrappedValue[T](containing: YType, instanceScope: Scope)
    extends Value(containing, instanceScope) {
  val v: T

  override def equals(obj: Any): Boolean = v.equals(obj.asInstanceOf[WrappedValue[T]].v)

  override def hashCode: Int = v.hashCode

  override def toString = String.valueOf(v)
}

abstract class WrappedComparableValue[T <: Comparable[T]](containing: YType, instanceScope: Scope)
    extends WrappedValue[T](containing, instanceScope)
    with Comparable[WrappedValue[T]] {

  override def compareTo(t: WrappedValue[T]) = v compareTo t.v

}
