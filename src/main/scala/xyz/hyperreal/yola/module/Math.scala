package xyz.hyperreal.yola.module

import xyz.hyperreal.yola._
import math._

object Math {

  val exports =
    YModule(
      Map(
        "cos" -> NativeFunction((args: List[Value]) =>
          args match {
            case List(YNumber(a)) => YNumber(BigDecimal(cos(a.toDouble)))
            case _                => perror("math.cos: expected a number")
        }),
        "pi" -> YNumber(BigDecimal(Pi))
      ))

}
