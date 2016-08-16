package func

import java.util.regex.Pattern
import func.FuncUtils.FuncString

import func.Operator.Operator

/**
  * Created by Henrik on 6/22/2016.
  */
case class CombinedFunction private[func](function1: Function, operator: Operator, function2: Function) extends Function {

  override def get(x: BigDecimal): BigDecimal = {
    operator match {
      case Operator.PLUS => function1.get(x) + function2.get(x)
      case Operator.MINUS => function1.get(x) - function2.get(x)
      case Operator.TIMES => function1.get(x) * function2.get(x)
      case Operator.DIVIDED_BY => function1.get(x) / function2.get(x)
    }
  }

  /**
    * derives the function using sum, product and quotient rule
    *
    * @return derivitive
    */
  override def derive(): Function = {
    operator match {
      case Operator.PLUS =>
        function1.derive() + function2.derive()
      case Operator.MINUS =>
        function1.derive() - function2.derive()
      case Operator.TIMES =>
        function1.derive() * function2 + (function1 * function2.derive())
      case Operator.DIVIDED_BY =>
        (function1.derive() * function2 - (function1 * function2.derive())) / function2.pow(2)
    }
  }

  override def scale(factor: BigDecimal) {
    operator match {
      case Operator.PLUS | Operator.MINUS =>
        function1.scale(factor)
        function2.scale(factor)
      case Operator.TIMES | Operator.DIVIDED_BY =>
        function1.scale(factor)
    }
  }

  /**
    * antiderives sums of functions, not possible for product/quotient of functions
    *
    * @param c integration constant
    * @return antiderivitive
    */
  override def antiderive(c: BigDecimal): Function = {
    operator match {
      case Operator.PLUS =>
        function1.antiderive(c) + function2.antiderive(c)
      case Operator.MINUS =>
        function1.antiderive(c) - function2.antiderive(c)
      case Operator.TIMES | Operator.DIVIDED_BY => ???
    }
  }

  override def get(x: Int): BigDecimal = super.get(x)

  override def toString: String = {
    var res = operator match {
      case Operator.PLUS =>
        function2 match {
          case polynomial: Polynomial =>
            if (polynomial.getFirstEffectiveScale < 0) {
              function1 + "" + function2
            } else {
              function1 + "+" + function2
            }
          case _ =>
            function1 + "+" + function2
        }
      case Operator.MINUS =>
        function1 + "-(" + function2 + ")"
      case Operator.TIMES | Operator.DIVIDED_BY =>
        "(" + function1 + ")" + Operator.string(operator) + "(" + function2 + ")"
    }
    val pm = Pattern.quote("+-")
    val mm = Pattern.quote("--")
    val pp = Pattern.quote("++")
    res = res.replaceAll(pm, "-").replaceAll(mm, "+").replaceAll(pp, "+")
    FuncUtils.removeRedundantBraces(res).trimOperators()
  }

  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[CombinedFunction]) return false
    val other = obj.asInstanceOf[CombinedFunction]
    function1.equals(other.function1) && operator.equals(other.operator) && function2.equals(other.function2)
  }
}

private[func] object Operator extends Enumeration {
  type Operator = Value
  val PLUS, MINUS, TIMES, DIVIDED_BY = Value

  def string(operator: Operator): String = {
    operator match {
      case PLUS => "+"
      case MINUS => "-"
      case TIMES => "*"
      case DIVIDED_BY => "/"
    }
  }
}
