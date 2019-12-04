package xyz.hyperreal.yola

class EnumType(val name: String) extends YType {
  val parent = YObject
}

case class Enum(name: String, ordinal: Int) extends (Any => Any) {

  def apply(v: Any) =
    v match {
      case "name"    => name
      case "ordinal" => ordinal
    }

  override def toString = s"$name<$ordinal>"
}
