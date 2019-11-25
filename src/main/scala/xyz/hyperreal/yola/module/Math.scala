package xyz.hyperreal.yola.module

import xyz.hyperreal.yola._
import math._

object Math {

  val exports =
    Map(
      "cos" -> ((args: List[Any]) =>
        args match {
          case List(a: BigDecimal) => BigDecimal(cos(a.toDouble))
          case _                   => perror("math.cos: expected a number")
        }),
      "pi" -> BigDecimal(Pi)
    )

}
