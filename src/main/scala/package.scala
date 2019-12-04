package xyz.hyperreal

import scala.collection.immutable.Range
import scala.util.parsing.input.Position

package object yola {
  type MapType = collection.immutable.Map[Value, Value]

  def perror(msg: String) = problem(null, msg)

  def problem(pos: Position, error: String) = {
    if (pos eq null)
      println(error)
    else
      println(s"${pos.line}: $error\n${pos.longString}")

    sys.exit(1)
  }

//  def display(v: Any): String =
//    v match {
//      case Constructor(typ, name, Nil)    => name
//      case Constructor(typ, name, fields) => fields mkString (s"$typ:$name(", ", ", ")")
//      case _                              => String.valueOf(v)
//    }

  def quoted(v: Value): String =
    v match {
      case YString(s) => s"""'${s replace ("'", "\\'")}'"""
      case _          => v.toString
    }

  val globalScope =
    new Scope(null) {
      bindings("None" -> YProduct(None),
               "yola" -> YModule(
                 Map(
                   "math" -> xyz.hyperreal.yola.module.Math.exports
                 )))
    }
}
