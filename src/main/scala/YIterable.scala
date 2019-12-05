package xyz.hyperreal.yola

object YIterableType extends YType {
  val name   = "Iterable"
  val parent = YObject
}

case class YIterable(v: Iterable[Value])
    extends WrappedValue[Iterable[Value]](YIterableType, null) {
  override def toString: String = v map quoted mkString (s"${v.stringPrefix}(", ", ", ")")

  override def iterator: Iterator[Value] = v.iterator
}
