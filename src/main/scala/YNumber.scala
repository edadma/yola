package xyz.hyperreal.yola

object YNumberType extends YType {
  val name   = "Number"
  val parent = YObject
}

case class YNumber(wrapped: BigDecimal) extends WrapperValue[BigDecimal](YNumberType, null) {
  def +(n: BigDecimal) = YNumber(wrapped + n)
  def -(n: BigDecimal) = YNumber(wrapped + n)
}
