package func

import func.FuncUtils.MathString

/**
  * Created by Henrik on 6/22/2016.
  */
case class Func2Pow private[func](function: Function, n: Int) extends Function {

  var scaleFactor: BigDecimal = 1.0

  override def get(x: BigDecimal): BigDecimal = {
    val innerX = function.get(x)
    require(!(innerX == 0 && n == 0), "0^0 is undefined!")
    innerX.pow(n) * scaleFactor
  }

  /**
    * derives the function using power rule
    *
    * @return derivitive
    */
  override def derive(): Function = {
    val func = Func2Pow(function, n - 1)
    func.scale(scaleFactor * n)
    function.derive() * func
  }

  override def scale(factor: BigDecimal) {
    scaleFactor *= factor
  }

  /**
    * antiderives the function using chain rule
    *
    * @param c integration constant
    * @return antiderivitive
    */
  override def antiderive(c: BigDecimal): Function = {
    if (!function.isInstanceOf[Polynomial] || !function.asInstanceOf[Polynomial].isLinear) {
      throw new UnsupportedOperationException("Antideriving only possible if inner function is linear!")
    }
    val func = Func2Pow(function, n + 1)
    func.scale(scaleFactor / (n + 1))
    func / function.derive() + c
  }

  override def toString: String = {
    scaleFactor.toScalarString + "(" + function + ")^" + n
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case func: Func2Pow =>
        scaleFactor == func.scaleFactor && function.equals(func.function) && n == func.n
      case _ => false
    }
  }
}
