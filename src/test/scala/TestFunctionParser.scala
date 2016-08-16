import func.{FunctionParser, Polynomial}
import org.scalatest.FunSuite

/**
  * Created by Henrik on 7/9/2016.
  */
class TestFunctionParser extends FunSuite {

  val fp = FunctionParser

  var s1 = "-1.0*x^4"
  var s2 = "6.87x^1"
  var s3 = "x"

  test("testReadPow") {
    assertResult(4)(fp.readPow(s1, s1.indexOf('x') + 1))
    assertResult(1)(fp.readPow(s2, s2.indexOf('x') + 1))
    assertResult(1)(fp.readPow(s3, s3.indexOf('x') + 1))
  }

  test("testReadScalar") {
    assertResult(-1.0)(fp.readScalar(s1, s1.indexOf('x') - 1))
    assertResult(6.87)(fp.readScalar(s2, s2.indexOf('x') - 1))
    assertResult(1.0)(fp.readScalar(s3, s3.indexOf('x') - 1))
  }

  test("testParsePolynomial") {
    val func1 = fp.parsePolynomial(s1)
    val func2 = fp.parsePolynomial(s2)
    val func3 = fp.parsePolynomial(s3)
    assertResult(Array(0.0, 0.0, 0.0, 0.0, -1.0))(func1.scales)
    assertResult(Array(0.0, 6.87))(func2.scales)
    assertResult(Array(0.0, 1.0))(func3.scales)
    println(func1)
    println(func2)
    println(func3)
  }

}
