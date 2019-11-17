package xyz.hyperreal.yola

class YClass(val name: String, constructor: ExpressionAST, outer: Scope) {

  def instance: YObject = {
    implicit val scope = new Scope(outer)

    new Interpreter(null)(constructor)
    new YObject(this, scope)
  }
}
