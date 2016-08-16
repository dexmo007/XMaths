import func.Polynomial
import org.scalatest.FunSuite

/**
  * Created by Henrik on 6/20/2016.
  */
class TestXMath extends FunSuite {

  val pi = Math.PI

  test("sin") {
    val xes: Array[Double] = Array(0, pi / 6, pi / 4, pi / 3, pi / 2, pi, 3 * pi / 2, 2 * pi)
    val sins: Array[Double] = Array(0, 0.5, 1.0 / Math.sqrt(2), Math.sqrt(3) / 2, 1, 0, -1, 0)
    for (i <- 0 to 7) {
      assertResult(XMath.round(sins(i), 14))(XMath.sin(xes(i)))
    }
  }

  test("simplepolynom") {
    val polynom = new Polynomial(1.0, 1.0, 1.0)
    val res = Array(1.0, 3.0, 7.0, 13.0)
    for (x <- 0 to 3) {
      assertResult(res(x))(polynom.get(x))
    }
  }

  test("smallpolynom") {
    val polynom = new Polynomial(5.0, 2.0, 1.0)
    val xes = Array(-1.0, 0.0, 0.5, 1.0, 2.0)
    val res = Array(4.0, 5.0, 6.25, 8.0, 13.0)
    for (i <- 0 to 4) {
      assertResult(res(i))(polynom.get(xes(i)))
    }
  }

  test("combinedfunction") {
    val func = new Polynomial(1.0) + new Polynomial(2.0)
    assertResult(3.0)(func.get(0.0))
    val func2 = func - new Polynomial(4.0, 1.0)
    assertResult(-3.0)(func2.get(2.0))
  }
}
