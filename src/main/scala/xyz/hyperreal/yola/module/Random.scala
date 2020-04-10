package xyz.hyperreal.yola.module

import xyz.hyperreal.yola._
import math._
import util.{Random => R}

object Random {

  val exports =
    Module(
      Map(
        "number" -> NativeFunction((args: List[Value]) =>
          args match {
            case Nil => YNumber(R.nextDouble)
            case _   => perror("random.number: no parameter")
        }),
        "integer" -> NativeFunction((args: List[Value]) =>
          args match {
            case List(YNumber(a)) if a.isValidInt && a > 0 => YNumber(R.nextInt(a.intValue))
            case _                                         => perror("random.integer: expected positive integer")
        }),
      ))

}
