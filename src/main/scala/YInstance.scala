package xyz.hyperreal.yola

class YInstance(val instantiator: YInstantiableType, instanceScope: Scope) {

  def instanceOf(typ: YType) = typ == instantiator || instantiator.descendantOf(typ)

  def member(name: String) = instanceScope(name)
}
