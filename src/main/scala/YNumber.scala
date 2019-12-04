package xyz.hyperreal.yola

object YNumberType extends YType {
  val name   = "Number"
  val parent = YObject
}

case class YNumber(v: BigDecimal) extends WrappedValue[BigDecimal](YNumberType, null) {
  def +(n: BigDecimal) = YNumber(v + n)
  def -(n: BigDecimal) = YNumber(v + n)
}
