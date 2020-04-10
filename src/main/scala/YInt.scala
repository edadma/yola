package xyz.hyperreal.yola

object YIntType extends YType {
  val name   = "Int"
  val parent = YNumericType
}

case class YInt(v: Int) extends WrappedValue[Int](YIntType, null) {
  def +(n: Int) = YInt(v + n)
  def -(n: Int) = YInt(v - n)
}
