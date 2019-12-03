package xyz.hyperreal.yola

object YMapType extends YType {
  val name   = "Map"
  val parent = YObject
}

case class YMap(wrapped: MapType) extends WrapperValue[MapType](YMapType, null)
