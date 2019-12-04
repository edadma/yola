package xyz.hyperreal.yola

object UnitType extends YType {
  val name   = "Unit"
  val parent = YObject
}

object YUnit extends WrappedValue[Unit](UnitType, null) {
  val v = ()
}
