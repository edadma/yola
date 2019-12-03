package xyz.hyperreal.yola

object YRangeType extends YType {
  val name   = "Range"
  val parent = YObject
}

case class YRange(wrapped: Range) extends WrapperValue[Range](YRangeType, null)
