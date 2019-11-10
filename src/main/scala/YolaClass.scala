package xyz.hyperreal.yola

class YolaClass(val name: String, constructor: ExpressionAST, outer: Scope) {

  def instance: YolaObject = {
    implicit val scope = new Scope(outer)

    YolaInterpreter(constructor)
    new YolaObject(this, scope)
  }
}
