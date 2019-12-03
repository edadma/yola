package xyz.hyperreal.yola

object YNumberType extends YType {
  val name   = "Number"
  val parent = YObject
}

case class YNumber(primitive: BigDecimal) extends PrimitiveValue[BigDecimal](YNumberType, null)
