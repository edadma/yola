package xyz.hyperreal.yola

object YBooleanType extends YType {
  val name   = "Boolean"
  val parent = YObject
}

case class YBoolean(primitive: Boolean) extends PrimitiveValue[Boolean](YBooleanType, null)
