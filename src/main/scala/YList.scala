package xyz.hyperreal.yola

object YListType extends YType {
  val name   = "List"
  val parent = YIterableType
}

case class YList(v: List[Value]) extends WrappedValue[List[Value]](YListType, null) {
  override def toString: String = v map quoted mkString ("[", ", ", "]")
}
