package func.trig

import func.FuncUtils.MathString
import func.Function

/**
  * Created by Henrik on 6/25/2016.
  */
case class CosineFunction private[func](scale: BigDecimal) extends Function {

  var scaleFactor: BigDecimal = scale

  override def get(x: BigDecimal): BigDecimal = {
    scaleFactor * Math.cos(x.toDouble)
  }

  override def derive(): Function = {
    Function.sin(-scaleFactor)
  }

  override def scale(factor: BigDecimal): Unit = {
    scaleFactor *= factor
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.sin(scaleFactor) + c
  }

  override def toString: String = {
    scaleFactor.toScalarString + "cos(x)"
  }
}
