package func

/**
  * Created by Henrik on 6/25/2016.
  */
case class MethodFunction private[func](method: (BigDecimal) => BigDecimal) extends Function {

  var scale: BigDecimal = 1.0

  override def get(x: BigDecimal): BigDecimal = {
    scale * method(x)
  }

  override def derive(): Function = {
    throw new UnsupportedOperationException
  }

  override def scale(factor: BigDecimal): Unit = {
    scale *= factor
  }

  override def antiderive(c: BigDecimal): Function = {
    throw new UnsupportedOperationException
  }
}
