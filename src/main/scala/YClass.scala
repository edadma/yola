package xyz.hyperreal.yola

class YClass(
    val name: String,
    val parent: YType,
    constr: ConstructorAST,
    scope: Scope,
    interp: Interpreter
) extends YInstantiableType {

  def instantiate(args: Any*): Value = {
    implicit val instanceScope = new Scope(scope)

    interp(constr)
    new Value(this, instanceScope)
  }
}
