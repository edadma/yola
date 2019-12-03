package xyz.hyperreal.yola

object YFunctionType extends YType {
  val name   = "Number"
  val parent = YObject
}

case class YFunction(wrapped: FunctionExpressionAST)
    extends WrapperValue[FunctionExpressionAST](YNumberType, null)
