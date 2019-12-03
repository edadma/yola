package xyz.hyperreal.yola

object YStringType extends YType {
  val name   = "String"
  val parent = YObject
}

case class YString(wrapped: String) extends WrapperValue[String](YStringType, null) {
  def append(s: String)  = YString((new StringBuilder(wrapped) ++= s).toString)
  def prepend(s: String) = YString((new StringBuilder(s) ++= wrapped).toString)
}
