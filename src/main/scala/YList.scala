package xyz.hyperreal.yola

object YListType extends YType {
  val name   = "List"
  val parent = YObject
}

case class YList(v: List[Value]) extends WrappedValue[List[Value]](YListType, null) {
  override def toString: String = v map quoted mkString ("[", ", ", "]")
}
