package xyz.hyperreal.yola

object YTupleType extends YType {
  val name   = "Tuple"
  val parent = YObject
}

case class YTuple(v: List[Value]) extends WrappedValue[List[Value]](YTupleType, null) {
  override def toString: String = v map quoted mkString ("(", ", ", ")")
}
