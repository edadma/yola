package xyz.hyperreal.yola

object YRangeType extends YType {
  val name   = "Range"
  val parent = YObject
}

case class YRange(primitive: Range) extends PrimitiveValue[Range](YRangeType, null)
