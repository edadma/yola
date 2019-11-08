package xyz.hyperreal.yola

class YolaClass(val name: String, constructor: ExpressionAST) {

  def instance: YolaObject = {
    implicit val scope = new Scope

    YolaInterpreter(constructor)
    new YolaObject(this, scope)
  }
}
