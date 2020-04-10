package xyz.hyperreal

import scala.util.parsing.input.Position

package object yola {

  type MapType = collection.immutable.Map[Value, Value]

  implicit def int2number(n: Int) = YNumber(n)

  def perror(msg: String) = problem(null, msg)

  def problem(pos: Position, error: String) = {
    if (pos eq null)
      println(error)
    else
      println(s"${pos.line}: $error\n${pos.longString}")

    sys.exit(1)
  }

  def quoted(v: Value): String =
    v match {
      case YString(s) => s"""'${s replace ("'", "\\'")}'"""
      case _          => v.toString
    }

  val globalScope =
    new Scope(null) {
      bindings(
//        "None" -> YProduct(None), // todo: implement None/Some correctly
        "yola" -> Module(
          Map(
            "math" -> xyz.hyperreal.yola.module.Math.exports
          )
        )
      )
    }

}
