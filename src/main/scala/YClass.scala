package xyz.hyperreal.yola

class YClass(
    val name: String,
    val parent: YType,
    constructor: List[StatementAST],
    scope: Scope,
    interp: Interpreter
) extends YInstantiableType {

  def instantiate(args: Any*): Value = {
    implicit val instanceScope = new Scope(scope)

    interp(constructor)
    new Value(this, instanceScope)
  }
}
