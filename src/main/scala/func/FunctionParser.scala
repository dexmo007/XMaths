package func

import scala.collection.mutable
import func.FuncUtils.MathString
import func.FunctionParser.BraceString

/**
  * Created by Henrik on 7/8/2016.
  */
object FunctionParser {
  private val validChars = "+-*^x."
  private val validOps = "+-*/^"

  def evaluateParen(s: String): BigDecimal = {
    // ValidateParentheses , only need once actually
    val openPar = s.indexOf("(")
    if (openPar != -1) {
      val closePar = findCloseParen(s, openPar)

    }
    0
  }

  def evaluate2(s: String): BigDecimal = {
    if (!s.contains('+') && !s.contains('-')) {
      evaluate(s)
    }
    val ops = new Array[Char](0).toBuffer
    val terms = new Array[Any](0).toBuffer
    var lastOpIndex = -1
    var i = 0
    while (i < s.length) {
      if (s(i) == '(') {
        i = findCloseParen(s, i)
      } else if (validOps.contains(s(i))) {
        terms.append(s.substring(lastOpIndex + 1, i).trimBraces())
        ops.append(s(i))
        lastOpIndex = i
      }
      i += 1
    }
    terms.append(s.substring(lastOpIndex + 1, s.length).trimBraces())
    perform(ops, terms, "^")
    perform(ops, terms, "*/")
    perform(ops, terms, "+-")
    terms.head match {
      case d: BigDecimal => d
      case s: String => evaluate2(s)
    }
  }

  def perform(ops: mutable.Buffer[Char], terms: mutable.Buffer[Any], op: String): Unit = {
    var i = 0
    while (i < ops.length) {
      if (op.contains(ops(i))) {
        val term1 = terms(i) match {
          case s: String => evaluate2(s)
          case d: BigDecimal => d
        }
        val term2 = terms(i + 1) match {
          case s: String => evaluate2(s)
          case d: BigDecimal => d
        }
        terms(i) = getOperation(ops(i))(term1, term2)
        ops.remove(i)
        terms.remove(i + 1)
      } else {
        i += 1
      }
    }
  }

  implicit class BraceString(s: String) {
    def trimBraces(): String = {
      var res = s
      if (res.head == '(') {
        res = res.substring(1)
      }
      if (res.last == ')') {
        res = res.substring(0, res.length - 1)
      }
      res
    }
  }

  def getOperation(c: Char): (BigDecimal, BigDecimal) => BigDecimal = {
    c match {
      case '+' =>
        (bd1: BigDecimal, bd2: BigDecimal) => bd1 + bd2
      case '-' =>
        (bd1: BigDecimal, bd2: BigDecimal) => bd1 - bd2
      case '*' =>
        (bd1: BigDecimal, bd2: BigDecimal) => bd1 * bd2
      case '/' =>
        (bd1: BigDecimal, bd2: BigDecimal) => bd1 / bd2
      case '^' =>
        (bd1: BigDecimal, bd2: BigDecimal) => Math.pow(bd1.toDouble, bd2.toDouble)
      case _ =>
        throw FunctionParseException("Invalid character: " + c)
    }
  }

  def evaluate(s: String): BigDecimal = {
    if (s == "e" || s == "E") {
      Math.E
    } else if (s == "pi" || s == "Pi" || s == "PI") {
      Math.PI
    } else if (s.isEmpty) {
      0
    } else {
      try {
        BigDecimal(s)
      } catch {
        case _: Exception => throw new IllegalArgumentException("String invalid!")
      }
    }
  }

  def parse(s: String): Function = {
    val str = s.trim
    for (i <- str.indices) {
      if (str(i).isLetter && str(i) != 'x') {
        var j = i
        while (j < str.length && str(j) != '(') {
          j += 1
        }
        val scalar: BigDecimal = readScalar(str, i - 1)
        // found a function call
        val outer = getFunction(str.substring(i, j))(scalar)
        //        val scale =
        val closePar = findCloseParen(str, j)
        val innerString = str.substring(j + 1, closePar)
        val innerFunc: Function = parse(innerString)
        var funcCall = outer.of(innerFunc)
        // todo single power gets simply fied
        if (innerFunc.equals(Function.linear(1))) {
          funcCall = outer
        }
        return parse(str.substring(0, i)) + funcCall + parse(str.substring(closePar + 1))
      }
    }
    parsePolynomial(str)
  }

  def findCloseParen(s: String, open: Int): Int = {
    val stack = new mutable.Stack[Char]
    var i = open
    while (i < s.length) {
      val c = s(i)
      c match {
        case '(' => stack.push(c)
        case ')' => stack.pop()
        case _ =>
      }
      if (stack.isEmpty) {
        return i
      }
      i += 1
    }
    throw FunctionParseException("No closing parentheses!")
  }

  /**
    *
    * parser for string like "3*x&#94;2+4x+5"
    *
    * @param s
    * @return
    */
  def parsePolynomial(s: String): Polynomial = {
    //    checkString(s)
    var map = mutable.Map[Int, BigDecimal]()
    // iterates thru all summands that scale x^n
    for (i <- s.indices) {
      if (s(i) == 'x') {
        val pow = readPow(s, i + 1)
        val scalar = readScalar(s, i - 1)
        if (map.contains(pow)) {
          map(pow) += scalar
        } else {
          map += pow -> scalar
        }
      }
    }
    // get scale of x^0
    map += 0 -> getSimpleSummand(s)

    val level = map.keysIterator.max
    val scls: Array[BigDecimal] = Array.fill(level + 1) {
      0.0
    }
    map.foreach(entry => scls(entry._1) = entry._2)
    Function.polynomial(scls: _*).asInstanceOf[Polynomial]
  }

  def getSimpleSummand(s: String): BigDecimal = {
    var sum: BigDecimal = 0
    for (str <- s.split('+')) {
      if (!str.isEmpty && !str.contains('x')) {
        //        println(str)
        sum += BigDecimal(str.trim)
      } else if (str.contains('-')) {
        val split = str.split('-')
        if (!split(0).contains('x')) {
          sum += BigDecimal(split(0))
        }
        for (i <- 1 until split.length) {
          if (!split(i).contains('x')) {
            sum -= BigDecimal(split(i))
          }
        }
      }
    }
    sum
  }

  def checkString(s: String): Unit = {
    // todo incomplete
    for (c <- s) {
      if (!validChars.contains(c) && !c.isDigit) {
        throw FunctionParseException("String contains invalid character: " + c)
      }
    }
  }

  def readPow(s: String, index: Int): Int = {
    if (index >= s.length || s(index) != '^') {
      return 1
    }
    var i = index + 1
    val sb = StringBuilder.newBuilder
    while (i < s.length && s(i).isDigit) {
      sb += s(i)
      i += 1
    }
    sb.toInt
  }

  def readScalar(s: String, index: Int): BigDecimal = {
    var i = index
    val sb = StringBuilder.newBuilder
    // no scalar = 1 v -1
    if (i < 0 || s(i) == '+') {
      return 1
    }
    if (s(i) == '-') {
      return -1
    }
    // multiply sign
    if (s(i) == '*') {
      i -= 1
    }
    // read all digits
    while (i >= 0 && (s(i).isDigit || s(i) == '.')) {
      sb += s(i)
      i -= 1
    }
    val num = BigDecimal(sb.reverse.toString)
    // determine sign
    if (i < 0 || s(i) == '+') {
      num
    } else if (s(i) == '-') {
      -num
    } else {
      throw FunctionParseException("Invalid character before scalar: " + s(i))
    }
  }

  def getFunction(symbol: String): (BigDecimal) => Function = {
    symbol match {
      case "ln" => (scale: BigDecimal) => Function.ln(scale)
      case "exp" => (scale: BigDecimal) => Function.exp(scale)
      case "sin" => (scale: BigDecimal) => Function.sin(scale)
      case "asin" => (scale: BigDecimal) => Function.asin(scale)
      case "cos" => (scale: BigDecimal) => Function.cos(scale)
      case "acos" => (scale: BigDecimal) => Function.acos(scale)
      case "tan" => (scale: BigDecimal) => Function.tan(scale)
      case "atan" => (scale: BigDecimal) => Function.atan(scale)
      case "cot" => (scale: BigDecimal) => Function.cot(scale)
      case "acot" => (scale: BigDecimal) => Function.acot(scale)
      case _ => throw FunctionParseException("No such function:" + symbol)
    }
  }

  case class FunctionParseException(msg: String) extends Exception(msg)

  def main(args: Array[String]) {
    println(evaluate2("1+2").toMathString)
  }

}
