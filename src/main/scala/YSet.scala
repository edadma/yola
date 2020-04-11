package xyz.hyperreal.yola

object YSetType extends YType {

  val name   = "Set"
  val parent = YIterableType

}

case class YSet(v: Set[Value]) extends WrappedValue[Set[Value]](YSetType, null) {

  override def toString: String = v map quoted mkString ("{", ", ", "}")

  override def iterator: Iterator[Value] = v.iterator

}
