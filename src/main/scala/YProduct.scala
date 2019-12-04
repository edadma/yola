package xyz.hyperreal.yola

object YProductType extends YType {
  val name   = "Product"
  val parent = YObject
}

case class YProduct(v: Product) extends WrappedValue[Product](YProductType, null) {
  override def toString =
    if (v.productArity == 0)
      v.productPrefix
    else {
      val prefix = if (v.productPrefix.matches("Tuple[0-9]+")) "(" else v.productPrefix

      v.productIterator.asInstanceOf[Iterator[Value]] map quoted mkString (s"$prefix(", ", ", ")")
    }
}
