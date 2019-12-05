package xyz.hyperreal.yola

object YRangeType extends YType {
  val name   = "Range"
  val parent = YIterableType
}

case class YRange(start: BigDecimal, end: BigDecimal, step: BigDecimal, inclusive: Boolean)
    extends Value(YRangeType, null) {
  require(step != 0, "step should not be zero")

  override def toString: String =
    s"Range(start: $start, end: $end, step: $step, inclusive: $inclusive)"

  override def iterator: Iterator[YNumber] =
    new Iterator[YNumber] {
      var idx = start

      def hasNext =
        (step > 0, inclusive) match {
          case (false, false) => idx > end
          case (true, false)  => idx < end
          case (false, true)  => idx >= end
          case (true, true)   => idx <= end
        }

      def next =
        if (hasNext) {
          val res = YNumber(idx)

          idx += step
          res
        } else
          throw new NoSuchElementException("no more range elements")
    }
}
