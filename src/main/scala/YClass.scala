package xyz.hyperreal.yola

class YClass(
    val name: String,
    val parent: YType,
    constructor: ExpressionAST,
    definingScope: Scope,
    global: Scope
) extends YInstantiableType {

  def instantiate(args: Any*): Value = {
    implicit val instanceScope = new Scope(definingScope)

    new Interpreter(global)(constructor)
    new Value(this, instanceScope)
  }
}
