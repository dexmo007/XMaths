package func

import func.exp.{EFunction, ExponentialFunction}
import func.log.{LnFunction, LogBaseFunction}
import func.trig._

/**
  * Created by Henrik on 6/20/2016.
  */
trait Function {

  def get(x: BigDecimal): BigDecimal

  def get(x: Int): BigDecimal = get(BigDecimal(x))

  def scale(factor: BigDecimal)

  def derive(): Function

  def antiderive(c: BigDecimal): Function

  def antiderive(): Function = antiderive(0.0)

  def integral(upper: BigDecimal, lower: BigDecimal): BigDecimal = {
    val antideriv = antiderive()
    antideriv.get(upper) - antideriv.get(lower)
  }

  def +(that: Function): Function = {
    CombinedFunction(this, Operator.PLUS, that)
  }

  def -(that: Function): Function = {
    CombinedFunction(this, Operator.MINUS, that)
  }

  def *(that: Function): Function = {
    CombinedFunction(this, Operator.TIMES, that)
  }

  def /(that: Function): Function = {
    CombinedFunction(this, Operator.DIVIDED_BY, that)
  }

  def pow(n: Int): Function = {
    Func2Pow(this, n)
  }

  def of(inner: Function): Function = {
    ConcatFunction(this, inner)
  }
}

object Function {

  def cos(scale: BigDecimal): Function = CosineFunction(scale)

  def cos(): Function = CosineFunction(1)

  def acos(scale: BigDecimal): Function = ArccosineFunction(scale)

  def acos(): Function = ArccosineFunction(1)

  def sin(scale: BigDecimal): Function = SineFunction(scale)

  def sin(): Function = SineFunction(1)

  def asin(scale: BigDecimal): Function = ArcsineFunction(scale)

  def asin(): Function = ArcsineFunction(1)

  def tan(scale: BigDecimal): Function = TangentFunction(scale)

  def tan(): Function = TangentFunction(1)

  def atan(scale: BigDecimal): Function = ArctangentFunction(scale)

  def atan(): Function = ArctangentFunction(1)

  def cot(scale: BigDecimal): Function = scale / TangentFunction(1)

  def cot(): Function = 1 / TangentFunction(1)

  def acot(scale: BigDecimal): Function = ArccotangentFunction(scale)

  def acot(): Function = ArccotangentFunction(1)

  def ln(scale: BigDecimal): Function = LnFunction(scale)

  def ln(): Function = LnFunction(1)

  def log10(scale: BigDecimal): Function = LogBaseFunction(10, scale)

  def log10(): Function = LogBaseFunction(10, 1)

  def logb(base: Int, scale: BigDecimal) = LogBaseFunction(base, scale)


  def logb(base: Int): Function = LogBaseFunction(base, 1)

  def linear(b: BigDecimal, a: BigDecimal): Function = Polynomial(b, a)

  def linear(a: BigDecimal): Function = Polynomial(0, a)

  def const(c: BigDecimal): Function = Polynomial(c)

  def toPolynomial(scls: Array[BigDecimal]): Function = Polynomial(scls: _*)

  def toPolynomial(scls: Array[Double]): Function = Polynomial(scls.map(d => BigDecimal(d)): _*)

  def toPolynomial(scls: Array[Int]): Function = Polynomial(scls.map(i => BigDecimal(i)): _*)

  def polynomial(scls: BigDecimal*): Function = Polynomial(scls: _*)

  def from(method: (BigDecimal) => BigDecimal): Function = MethodFunction(method)

  def sqrt(scale: BigDecimal): Function = RootFunction(2, scale)

  def sqrt(): Function = RootFunction(2, 1)

  def cbrt(scale: BigDecimal): Function = RootFunction(3, scale)

  def cbrt(): Function = RootFunction(3, 1)

  def nthRoot(n: BigDecimal, scale: BigDecimal): Function = RootFunction(n, scale)

  def nthRoot(n: BigDecimal): Function = RootFunction(n, 1)

  def exp(scale: BigDecimal, function: Function): Function = EFunction(function, scale)

  def exp(function: Function): Function = EFunction(function, 1)

  def exp(scale: BigDecimal): Function = EFunction(linear(1), scale)

  def exp(): Function = EFunction(linear(1), 1)

  def expb(scale: BigDecimal, base: BigDecimal, function: Function): Function = ExponentialFunction(base, function, scale)

  def expb(scale: BigDecimal, base: BigDecimal): Function = ExponentialFunction(base, linear(1), scale)

  def expb(base: BigDecimal, function: Function): Function = ExponentialFunction(base, function, 1)

  def expb(base: BigDecimal): Function = ExponentialFunction(base, linear(1), 1)

  implicit def bigDecimalToPolynom(num: BigDecimal): Function = Polynomial(num)

  implicit def intToPolynom(num: Int): Function = Polynomial(num)
}
