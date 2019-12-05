package xyz.hyperreal.yola

object NativeFunctionType extends YType {
  val name   = "NativeFunction"
  val parent = YObject
}

case class NativeFunction(v: List[Value] => Value)
    extends WrappedValue[List[Value] => Value](NativeFunctionType, null)
