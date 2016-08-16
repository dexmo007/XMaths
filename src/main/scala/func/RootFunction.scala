package func

/**
  * Created by Henrik on 6/25/2016.
  */
case class RootFunction private[func](n: BigDecimal, scale: BigDecimal) extends Function {

  require(n != 0, "0th root does not exist!")

  var scl = scale

  override def get(x: BigDecimal): BigDecimal = {
    if (n == 2) {
      scl * Math.sqrt(x.toDouble)
    } else if (n == 3) {
      scl * Math.cbrt(x.toDouble)
    } else {
      scl * Math.pow(x.toDouble, (1.0 / n).toDouble)
    }
  }

  override def derive(): Function = {
    scl / RootFunction(n / (1 - n), n)
  }

  override def scale(factor: BigDecimal): Unit = {
    scl *= factor
  }

  override def antiderive(c: BigDecimal): Function = {
    RootFunction(n / (n + 1), scl * n / (n + 1)) + c
  }
}
