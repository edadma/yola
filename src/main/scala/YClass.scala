package xyz.hyperreal.yola

class YClass(val name: String,
             val parent: YType,
             constructor: ExpressionAST,
             definingScope: Scope,
             global: Scope)
    extends YInstantiableType {

  def instantiate(args: Any*): YInstance = {
    implicit val instanceScope = new Scope(definingScope)

    new Interpreter(global)(constructor)
    new YInstance(this, instanceScope)
  }
}
