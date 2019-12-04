package xyz.hyperreal.yola

import scala.collection.mutable
import scala.util.parsing.input.Position

class Scope(val outer: Scope) {
  private[yola] val decls = new mutable.HashMap[String, Value]
  private[yola] val types = new mutable.HashMap[String, YType]

  def apply(name: String) = decls(name)

  def bindings(binding: (String, Value)*): Unit = decls ++= binding

  def duplicate(pos: Position, name: String) =
    problem(pos, s"duplicate declaration: '$name'")

  def declare(pos: Position, name: String, value: Value) = {
    if (decls contains name)
      duplicate(pos, name)

    decls(name) = value
  }

  def get(name: String): Option[Value] =
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

object YFunctionsType extends YType {
  val name   = "Functions"
  val parent = YObject
}

case class Functions(v: mutable.HashMap[Int, FunctionExpressionAST])
    extends WrappedValue[mutable.HashMap[Int, FunctionExpressionAST]](YFunctionsType, null)
