import func.{Function, FunctionParser}

/**
  * Created by Henrik on 6/20/2016.
  */
object XMath {

  val sinDerivitives: Array[BigDecimal] = Array(0, 1, 0, -1)
  val pi: BigDecimal = Math.PI

  val accuracy = 50

  def sin(x: Double): Double = {
    // trim to range 0 to 2*pi
    val trimmedX = x % (2 * pi)
    var sum: BigDecimal = 0
    // calc with sum
    for (k <- 0 to accuracy) {
      val deriv = sinDerivitives(k % 4)
      val fact = BigDecimal.apply(factorial(k))
      val exp = trimmedX.pow(k)
      sum += deriv / fact * exp
    }
    round(sum, 14).toDouble
  }

  def factorial(x: Int): BigInt = {
    var fact: BigInt = 1
    for (i <- 1 to x) {
      fact *= i
    }
    fact
  }

  def round(x: BigDecimal, digits: Int): BigDecimal = {
    val factor = Math.pow(10, digits)
    BigDecimal(Math.round((x * factor).toDouble)) / factor
  }

  def main(args: Array[String]) {
    val func = Function.logb(-1)
    println(func.get(-1))
  }
}
