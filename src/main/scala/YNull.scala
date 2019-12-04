package xyz.hyperreal.yola

object NullType extends YType {
  val name   = "Null"
  val parent = AnyType
}

object YNull extends WrappedValue[Null](NullType, null) {
  val v = null
}
