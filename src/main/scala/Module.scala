package xyz.hyperreal.yola

object YModuleType extends YType {
  val name   = "Module"
  val parent = YObject
}

case class YModule(v: Map[String, Value])
    extends WrappedValue[Map[String, Value]](YModuleType, null)
