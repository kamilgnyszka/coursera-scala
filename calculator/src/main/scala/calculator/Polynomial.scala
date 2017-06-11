package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bv = b()
      val av = a()
      val cv = c()
      Math.pow(bv,2.0) - 4.0*av*cv
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      val bv = b()
      val av = a()
      val cv = c()
      val dv = delta()

      Set((bv+Math.pow(dv,0.5))/(2.0*av),(bv-Math.pow(dv,0.5))/(2.0*av))
    }
  }
}
