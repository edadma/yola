package xyz.hyperreal.yola

object YStringType extends YType {
  val name   = "String"
  val parent = YObject
}

case class YString(primitive: String) extends PrimitiveValue[String](YStringType, null)
