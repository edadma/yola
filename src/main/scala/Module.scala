package xyz.hyperreal.yola

object ModuleType extends YType {
  val name   = "Module"
  val parent = YObject
}

case class Module(v: Map[String, Value])
    extends WrappedValue[Map[String, Value]](ModuleType, null)
