package xyz.hyperreal.yola

object YBooleanType extends YType {
  val name   = "Boolean"
  val parent = YObject
}

case class YBoolean(wrapped: Boolean) extends WrapperValue[Boolean](YBooleanType, null)
