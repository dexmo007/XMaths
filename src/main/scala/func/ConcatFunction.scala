package func

/**
  * Created by Henrik on 6/25/2016.
  */
case class ConcatFunction private[func](outer: Function, inner: Function) extends Function {

  override def get(x: BigDecimal): BigDecimal = {
    outer.get(inner.get(x))
  }

  override def derive(): Function = {
    inner.derive() * outer.derive().of(inner)
  }

  override def scale(factor: BigDecimal): Unit = {
    outer.scale(factor)
  }

  override def antiderive(c: BigDecimal): Function = ???

  override def toString: String = {
    if (outer.isInstanceOf[Polynomial]) {
      return outer.toString.replaceAll("x", "(" + inner + ")")
    }
    outer.toString.replaceAll("x", inner.toString)
  }
}
