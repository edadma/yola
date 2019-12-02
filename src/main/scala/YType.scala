package xyz.hyperreal.yola

abstract class YType {

  val name: String
  val parent: YType

  def descendantOf(typ: YType): Boolean = parent == typ || parent.descendantOf(typ)
}

abstract class YInstantiableType extends YType {

  def instantiate(args: Any*): YInstance
}

object YObject extends YInstantiableType {

  val name   = "Object"
  val parent = null

  def instantiate(args: Any*) = {
    new YInstance(YObject, null)
  }
}
