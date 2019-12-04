package xyz.hyperreal.yola

object YStringType extends YType {
  val name   = "String"
  val parent = YObject
}

case class YString(v: String) extends WrappedValue[String](YStringType, null) {
  def append(s: String)  = YString((new StringBuilder(v) ++= s).toString)
  def prepend(s: String) = YString((new StringBuilder(s) ++= v).toString)
}
