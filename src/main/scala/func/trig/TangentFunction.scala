package func.trig

import func.Function
import func.MethodFunction
import func.log.LnFunction

/**
  * Created by Henrik on 6/25/2016.
  */
case class TangentFunction private[func](scale: BigDecimal) extends Function {

  var scaleFactor = scale

  override def get(x: BigDecimal): BigDecimal = {
    scaleFactor * Math.tan(x.toDouble)
  }

  override def derive(): Function = {
    scaleFactor / Function.cos().pow(2)
  }

  override def scale(factor: BigDecimal): Unit = {
    scaleFactor *= factor
  }

  override def antiderive(c: BigDecimal): Function = {
    LnFunction(scaleFactor).of(MethodFunction(x => Math.abs(Math.cos(x.toDouble))))
  }
}
