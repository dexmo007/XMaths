package func.log

import func.Function

/**
  * Created by Henrik on 6/25/2016.
  */
case class LnFunction private[func](scale: BigDecimal) extends Function {

  var scaleFactor: BigDecimal = scale

  override def get(x: BigDecimal): BigDecimal = {
    require(x > 0, "ln(" + x + ") is undefined!")
    scaleFactor * Math.log(x.toDouble)
  }

  override def derive(): Function = {
    Function.const(scaleFactor) / Function.linear(1)
  }

  override def scale(factor: BigDecimal): Unit = {
    scaleFactor *= factor
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.linear(scaleFactor) * (LnFunction(1) - 1) + c
  }

  override def toString: String = {
    if (scaleFactor == 1) {
      "ln(x)"
    } else if (scaleFactor == -1) {
      "-ln(x)"
    } else {
      scaleFactor + "*ln(x)"
    }
  }
}
