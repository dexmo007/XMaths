package func.exp

import func.{Function, Polynomial}

/**
  * Created by Henrik on 6/27/2016.
  */
case class ExponentialFunction private[func](base: BigDecimal, inner: Function, scale: BigDecimal) extends Function {

  var scl = scale

  override def get(x: BigDecimal): BigDecimal = {
    scl * Math.pow(base.toDouble, inner.get(x).toDouble)
  }

  override def derive(): Function = {
    val innerDeriv: Function = inner.derive()
    innerDeriv.scale(Math.log(base.toDouble))
    innerDeriv * this
  }

  override def scale(factor: BigDecimal): Unit = {
    scl *= factor
  }

  override def antiderive(c: BigDecimal): Function = {
    if (!inner.isInstanceOf[Polynomial]) throw new UnsupportedOperationException
    val func = inner.asInstanceOf[Polynomial]
    if (!func.isLinear) throw new UnsupportedOperationException
    if (func.scales(1) == 0) {
      Polynomial(c, scl * Math.pow(base.toDouble, func.scales(0).toDouble))
    } else {
      ExponentialFunction(base, inner, scl / (func.scales(1) * Math.log(base.toDouble)))
    }
  }
}
