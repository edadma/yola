package xyz.hyperreal.yola

import scala.collection.mutable
import scala.util.parsing.input.Position

class Scope(val outer: Scope) {
  val vars = new mutable.HashMap[String, Any]

  def duplicate(pos: Position, name: String) =
    problem(pos, s"duplicate declaration: '$name'")

  def declare(pos: Position, name: String, value: Any) = {
    if (vars contains name)
      duplicate(pos, name)

    vars(name) = value
  }

  def get(name: String): Option[Any] =
    vars get name orElse (if (outer eq null) None else outer.get(name))

  def add(pos: Position, name: String, piece: FunctionPieceAST)(implicit scope: Scope) = {
    vars get name match {
      case None =>
        val fexp = FunctionExpressionAST(List(piece))

        fexp.scope = scope
        vars(name) = Functions(
          mutable.HashMap[Int, FunctionExpressionAST](piece.parms.length -> fexp))
      case Some(Functions(map)) =>
        val fexp = FunctionExpressionAST(map get piece.parms.length match {
          case None    => List(piece)
          case Some(f) => f.pieces :+ piece
        })

        fexp.scope = scope
        map(piece.parms.length) = fexp
      case _ => duplicate(pos, name)
    }
  }

  override def toString: String = s"Scope:$vars"
}

case class Functions(map: mutable.HashMap[Int, FunctionExpressionAST])
