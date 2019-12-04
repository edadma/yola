package xyz.hyperreal.yola

object YBooleanType extends YType {
  val name   = "Boolean"
  val parent = YObject
}

case class YBoolean(v: Boolean) extends WrappedValue[Boolean](YBooleanType, null)
