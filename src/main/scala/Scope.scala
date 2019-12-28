package xyz.hyperreal.yola

import scala.collection.mutable
import scala.util.parsing.input.Position

class Scope(val outer: Scope) {
  private[yola] val values = new mutable.HashMap[String, Value]
  private[yola] val types  = new mutable.HashMap[String, YType]

  def apply(name: String) = values(name)

  def bindings(binding: (String, Value)*): Unit = values ++= binding

  def duplicate(pos: Position, name: String) =
    problem(pos, s"duplicate declaration: '$name'")

  def declare(pos: Position, name: String, value: Value) = {
    if (values contains name)
      duplicate(pos, name)

    values(name) = value
  }

  def typedef(pos: Position, name: String, typ: YType) = {
    if (values contains name)
      duplicate(pos, name)

    types(name) = typ
  }

  def getValue(name: String): Option[Value] =
    values get name orElse (if (outer eq null) None else outer.getValue(name))

  def getType(name: String): Option[YType] =
    types get name orElse (if (outer eq null) None else outer.getType(name))

  def addFunctionPiece(pos: Position, name: String, piece: FunctionPieceAST)(
      implicit scope: Scope) = {
    values get name match {
      case None =>
        val fexp = FunctionExpressionAST(List(piece))

        fexp.scope = scope
        values(name) = Functions(
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

  override def toString: String = s"Scope:$values"
}

object YFunctionsType extends YType {
  val name   = "Functions"
  val parent = YObject
}

case class Functions(v: mutable.HashMap[Int, FunctionExpressionAST])
    extends WrappedValue[mutable.HashMap[Int, FunctionExpressionAST]](YFunctionsType, null)
