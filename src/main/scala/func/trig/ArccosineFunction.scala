package func.trig

import func.{Function, Polynomial}

/**
  * Created by Henrik on 7/8/2016.
  */
case class ArccosineFunction private[func](scale: BigDecimal) extends Function {

  var scl = scale

  override def get(x: BigDecimal): BigDecimal = {
    scl * Math.acos(x.toDouble)
  }

  override def derive(): Function = {
    -scl / Function.sqrt().of(Polynomial(1, 0, -1))
  }

  override def scale(factor: BigDecimal): Unit = {
    scl *= factor
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.linear(scl) * ArccosineFunction(1) - Function.sqrt(scl).of(Polynomial(1, 0, -1)) + c
  }
}
