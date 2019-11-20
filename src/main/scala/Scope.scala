package xyz.hyperreal.yola

import scala.collection.mutable
import scala.util.parsing.input.Position

class Scope(val outer: Scope) {
  val vars = new mutable.HashMap[String, Any]
  //val funcs = new mutable.HashMap[Int, List[FunctionExpressionAST]]()

  def duplicate(pos: Position, name: String): Unit = {
    if (vars contains name)
      problem(pos, s"duplicate declaration: '$name'")
  }

  def declare(pos: Position, name: String, value: Any) = {
    duplicate(pos, name)
    vars(name) = value
  }

  def get(name: String): Option[Any] =
    vars get name orElse (if (outer eq null) None else outer.get(name))
}
