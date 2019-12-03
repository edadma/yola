package xyz.hyperreal.yola

object YTupleType extends YType {
  val name   = "Tuple"
  val parent = YObject
}

case class YTuple(wrapped: List[Any]) extends WrapperValue[List[Any]](YTupleType, null)
