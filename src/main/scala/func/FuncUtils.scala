package func

import org.apache.commons.math3.fraction.Fraction

/**
  * Created by Henrik on 7/11/2016.
  */
object FuncUtils {

  def toFraction(num: BigDecimal): String = {
    new Fraction(num.toDouble, 1e-15, 100).toString
  }

  def removeRedundantBraces(s: String): String = {
    var res = s
    var i = 0
    while (res.indexOf("(", i) != -1) {
      if (res.indexOf(")", i) == i + 2 && i > 0 && !res(i - 1).isLetter) {
        res = res.substring(0, i) + res(i + 1) + res.substring(i + 3)
      }
      i += 1
    }
    res
  }

  implicit class FuncString(s: String) {
    def trimOperators(): String = {
      var res = s
      if (res(0) == '+') {
        res = res.substring(1)
      }
      if (res(res.length - 1) == '+' || res(res.length - 1) == '-') {
        res = res.substring(0, res.length - 1)
      }
      res
    }
  }

  implicit class MathString(num: BigDecimal) {
    def toMathString: String = {
      if (num == 0) {
        "0"
      } else if (num == Math.PI) {
        "pi"
      } else if (num % Math.PI == 0) {
        (num / Math.PI).toMathString + "*pi"
      } else if (num == Math.E) {
        "e"
      } else if (num % Math.E == 0) {
        (num / Math.E).toMathString + "*e"
      } else if (num.isValidInt) {
        num.toInt.toString
      } else {
        val frac = new Fraction(num.toDouble, 1e-15, 100)
        if (frac.getDenominator > 1000) {
          num.toString
        } else {
          frac.toString.replaceAll(" ", "")
        }
      }
    }
    def toScalarString: String = {
      if (num == 1) {
        ""
      } else if (num == -1) {
        "-"
      } else {
        num.toMathString + "*"
      }
    }
  }

}
