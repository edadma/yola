package xyz.hyperreal.yola

import scala.collection.mutable
import scala.util.parsing.input.Position

class Scope(val outer: Scope) {
  private[yola] val decls = new mutable.HashMap[String, Any]
  private[yola] val types = new mutable.HashMap[String, YType]

  def apply(name: String) = decls(name)

  def bindings(binding: (String, Any)*): Unit = decls ++= binding

  def duplicate(pos: Position, name: String) =
    problem(pos, s"duplicate declaration: '$name'")

  def declare(pos: Position, name: String, value: Any) = {
    if (decls contains name)
      duplicate(pos, name)

    decls(name) = value
  }

  def get(name: String): Option[Any] =
    decls get name orElse (if (outer eq null) None else outer.get(name))

  def addFunctionPiece(pos: Position, name: String, piece: FunctionPieceAST)(
      implicit scope: Scope) = {
    decls get name match {
      case None =>
        val fexp = FunctionExpressionAST(List(piece))

        fexp.scope = scope
        decls(name) = Functions(
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

  override def toString: String = s"Scope:$decls"
}

case class Functions(map: mutable.HashMap[Int, FunctionExpressionAST])
