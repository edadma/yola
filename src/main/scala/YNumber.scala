package xyz.hyperreal.yola

object YNumber extends YInstantiableType {
  val name   = "Number"
  val parent = YObject

  def instantiate(args: Any*) = {
    new YInstance(YNumber, null)
  }

}
