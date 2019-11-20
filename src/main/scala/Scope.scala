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

  def get(name: String, arity: Int): Option[Any] =
    get(name) map {
      case Functions(map) => map(arity)
      case v              => v
    }

  def add(pos: Position, name: String, piece: FunctionPieceAST) = {
    vars get name match {
      case None =>
        vars(name) = Functions(
          mutable.HashMap[Int, FunctionExpressionAST](
            piece.parms.length -> FunctionExpressionAST(List(piece))))
      case Some(Functions(map)) =>
        map(piece.parms.length) = FunctionExpressionAST(map get piece.parms.length match {
          case None    => List(piece)
          case Some(f) => f.pieces :+ piece
        })
      case _ => duplicate(pos, name)
    }
  }

  case class Functions(map: mutable.HashMap[Int, FunctionExpressionAST])
}
