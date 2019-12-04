package xyz.hyperreal.yola

object YMapType extends YType {
  val name   = "Map"
  val parent = YObject
}

case class YMap(v: MapType) extends WrappedValue[MapType](YMapType, null) {
  override def toString =
    v map { case (k, v) => s"${quoted(k)}: ${quoted(v)}" } mkString ("{", ", ", "}")
}
