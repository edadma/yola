package xyz.hyperreal.yola

abstract class YType {
  val name: String
  val parent: YType

  def descendantOf(typ: YType): Boolean =
    typ == AnyType || parent == typ || parent.descendantOf(typ)
}

abstract class YInstantiableType extends YType {
  def instantiate(args: Any*): Value
}

object AnyType extends YType {
  val name   = null
  val parent = null
}

object NullType extends YType {
  val name   = "Null"
  val parent = AnyType
}

object YObject extends YInstantiableType {
  val name   = "Object"
  val parent = null

  def instantiate(args: Any*) = {
    new Value(YObject, null)
  }
}
