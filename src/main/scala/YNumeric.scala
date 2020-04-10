package xyz.hyperreal.yola

import xyz.hyperreal.dal.Num

object YNumericType extends YType {
  val name   = "Numeric"
  val parent = YObject
}

case class YNumeric(v: Num) extends WrappedValue[Num](YNumericType, null) {
//  def +(n: BigDecimal) = YNumeric(v + n)
//  def -(n: BigDecimal) = YNumeric(v - n)
}
