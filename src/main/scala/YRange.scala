package xyz.hyperreal.yola

object YRangeType extends YType {
  val name   = "Range"
  val parent = YObject
}

case class YRange(v: Range) extends WrappedValue[Range](YRangeType, null) {
  override def toString: String =
    s"Range(start: ${v.start}, end: ${v.end}, step: ${v.step}, incl: ${v.isInclusive})"
}
