package xyz.hyperreal.yola

object YIterableType extends YType {
  val name   = "Iterable"
  val parent = YObject
}

case class YIterable(wrapped: Iterable[Any])
    extends WrapperValue[Iterable[Any]](YIterableType, null)
