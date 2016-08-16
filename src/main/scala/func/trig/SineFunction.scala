package func.trig

import func._
import func.FuncUtils.MathString

/**
  * Created by Henrik on 6/20/2016.
  */
case class SineFunction private[func](scale: BigDecimal) extends Function {

  var scaleFactor = scale

  override def get(x: BigDecimal): BigDecimal = {
    scaleFactor * Math.sin(x.toDouble)
  }

  override def derive(): Function = {
    Function.cos(scaleFactor)
  }

  override def scale(factor: BigDecimal): Unit = {
    scaleFactor *= factor
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.cos(-scaleFactor) + c
  }

  override def toString: String = {
    scaleFactor.toScalarString + "sin(x)"
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case sine: SineFunction =>
        // a*sin(x)=b*sin(x), if a=b
        this.scaleFactor == sine.scaleFactor
      case concat: ConcatFunction =>
        concat.outer match {
          case sine: SineFunction =>
            if (!concat.inner.isInstanceOf[Polynomial]) {
              return false
            }
            val inner = concat.inner.asInstanceOf[Polynomial]
            // a*sin(x) = a*sin(x + k*2pi), k is int
            (scaleFactor == sine.scaleFactor && inner.isLinear
              && inner.scales(0) % (Math.PI * 2) == 0 && inner.scales(1) == 1) ||
              // -a*sin(x) = a*sin(-x)
              (-scaleFactor == sine.scaleFactor && inner.isLinear
                && inner.scales(0) == 0 && inner.scales(1) == -1)
          case cosine: CosineFunction =>
            if (!concat.inner.isInstanceOf[Polynomial]) {
              return false
            }
            val inner = concat.inner.asInstanceOf[Polynomial]
            inner.isLinear &&
              // a*sin(x)=a*cos(pi/2-x)
              ((scaleFactor == cosine.scaleFactor && inner.scales(0) == Math.PI / 2 && inner.scales(1) == -1) ||
                // a*sin(x)= -a*cos(x+pi/2)
                (scaleFactor == -cosine.scaleFactor && inner.scales(0) == Math.PI / 2 && inner.scales(1) == 1) ||
                // a*sin(x) = a*cos(x-pi/2)
                (scaleFactor == cosine.scaleFactor && inner.scales(0) == -Math.PI / 2) && inner.scales(1) == 1)
          case _ =>
            false
        }
      case comb: CombinedFunction =>
        comb.operator == Operator.TIMES && (comb.function1 match {
          case tan: TangentFunction =>
            // a*sin(x)=b*tan(x)*c*cos(x), if b*c=a
            comb.function2.isInstanceOf[CosineFunction] &&
              comb.function2.asInstanceOf[CosineFunction].scaleFactor * tan.scaleFactor == scaleFactor
          case cos: CosineFunction =>
            // a*sin(x)=b*cos(x)*c*tan(x), if b*c=a
            comb.function2.isInstanceOf[TangentFunction] &&
              comb.function2.asInstanceOf[TangentFunction].scaleFactor * cos.scaleFactor == scaleFactor
          case _ => false
        })
      case _ =>
        false
    }
  }
}
