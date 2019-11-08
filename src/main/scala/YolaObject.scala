package xyz.hyperreal.yola

import scala.collection.immutable.HashMap

class YolaObject(clas: YolaClass, instanceScope: Scope) extends (String => Any) {
  def apply(name: String) = instanceScope.vars(name)
}
