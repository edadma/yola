package xyz.hyperreal

import scala.util.parsing.input.Position

package object yola {

  def problem(pos: Position, error: String) = {
    if (pos eq null)
      println(error)
    else
      println(s"${pos.line}: $error\n${pos.longString}")

    sys.exit(1)
  }

  def display(v: Any): String =
    v match {
      case NTuple(elems)     => elems map quotedDisplay mkString ("(", ", ", ")")
      case Record(con, args) => args map quotedDisplay mkString (s"${con.name}(", ", ", ")")
      case elems: List[_]    => elems map quotedDisplay mkString ("[", ", ", "]")
      case elems: collection.Seq[_] =>
        elems map quotedDisplay mkString (s"${elems.stringPrefix}(", ", ", ")")
      case map: collection.Map[_, _] =>
        map map { case (k, v) => s"${quotedDisplay(k)}: ${quotedDisplay(v)}" } mkString ("{", ", ", "}")
      case _ => String.valueOf(v)
    }

  def quotedDisplay(v: Any): String =
    v match {
      case s: String => s"""'${s replace ("'", "\\'")}'"""
      case _         => display(v)
    }

}
