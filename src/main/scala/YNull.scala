package xyz.hyperreal.yola

object NullType extends YType {
  val name   = "Null"
  val parent = AnyType
}

object YNull extends WrapperValue[Null](NullType, null) {
  val wrapped = null
}
