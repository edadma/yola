package xyz.hyperreal

import scala.collection.immutable.Range
import scala.util.parsing.input.Position

package object yola {
  def perror(msg: String) = problem(null, msg)

  def problem(pos: Position, error: String) = {
    if (pos eq null)
      println(error)
    else
      println(s"${pos.line}: $error\n${pos.longString}")

    sys.exit(1)
  }

  private val tupleRegex = "Tuple[0-9]+" r

  def display(v: Any): String =
    v match {
      case r: Range =>
        s"Range(start: ${r.start}, end: ${r.end}, step: ${r.step}, incl: ${r.isInclusive})"
      case NTuple(elems)     => elems map quotedDisplay mkString ("(", ", ", ")")
      case Record(con, args) => args.values map quotedDisplay mkString (s"${con.name}(", ", ", ")")
      case elems: List[_]    => elems map quotedDisplay mkString ("[", ", ", "]")
      case elems: collection.Seq[_] =>
        elems map quotedDisplay mkString (s"${elems.stringPrefix}(", ", ", ")")
      case map: collection.Map[_, _] =>
        map map { case (k, v) => s"${quotedDisplay(k)}: ${quotedDisplay(v)}" } mkString ("{", ", ", "}")
      case Constructor(typ, name, Nil)       => name
      case Constructor(typ, name, fields)    => fields mkString (s"$typ:$name(", ", ", ")")
      case Enum(name, ordinal)               => s"$name<$ordinal>"
      case p: Product if p.productArity == 0 => p.productPrefix
      case p: Product =>
        val prefix = if (p.productPrefix.matches("Tuple[0-9]+")) "(" else p.productPrefix

        p.productIterator map quotedDisplay mkString (s"$prefix(", ", ", ")")
      case _ => String.valueOf(v)
    }

  def quotedDisplay(v: Any): String =
    v match {
      case s: String => s"""'${s replace ("'", "\\'")}'"""
      case _         => display(v)
    }

  val globalScope =
    new Scope(null) {
      vars ++= Map(
        "None" -> None,
        "yola" -> Map(
          "math" -> xyz.hyperreal.yola.module.Math.exports
        )
      )
    }
}
