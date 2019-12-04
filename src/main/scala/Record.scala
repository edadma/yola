package xyz.hyperreal.yola

import scala.collection.mutable

case class Constructor(typ: String, name: String, fields: List[String])

object RecordType extends YType {
  val name   = "Record"
  val parent = YObject
}

case class Record(con: Constructor, args: mutable.LinkedHashMap[String, Value])
    extends Value(RecordType, null)
    with (Value => Value) {
  def apply(field: Value) = args(field.asInstanceOf[String])

  override def toString = args.values map quoted mkString (s"${con.name}(", ", ", ")")
}
