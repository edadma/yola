package xyz.hyperreal.yola

import scala.collection.immutable.HashMap

class YObject(clas: YClass, instanceScope: Scope) extends (String => Any) {
  def apply(name: String) = instanceScope.vars(name)
}
