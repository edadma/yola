package xyz.hyperreal.yola

object UnitType extends YType {
  val name   = "Unit"
  val parent = YObject
}

object YUnit extends WrapperValue[Unit](UnitType, null) {
  val wrapped = ()
}
