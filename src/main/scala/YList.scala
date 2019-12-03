package xyz.hyperreal.yola

object YListType extends YType {
  val name   = "List"
  val parent = YObject
}

case class YList(wrapped: List[Any]) extends WrapperValue[List[Any]](YListType, null)
