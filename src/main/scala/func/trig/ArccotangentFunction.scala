package func.trig

import func.{Function, Polynomial}

/**
  * Created by Henrik on 7/8/2016.
  */
case class ArccotangentFunction private[func](scale: BigDecimal) extends Function {
  var scl = scale

  override def get(x: BigDecimal): BigDecimal = {
    scl * Math.atan((1.0 / x).toDouble)
  }

  override def derive(): Function = {
    -scl / Function.sqrt().of(Polynomial(1, 0, 1))
  }

  override def scale(factor: BigDecimal): Unit = {
    scl *= factor
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.linear(scl) * ArccotangentFunction(1) + Function.ln(scl / 2).of(Polynomial(1, 0, 1))
  }
}
