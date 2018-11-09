package mgobench

package object utils {


  /**
    * Decorator for element by element sum
    * @param v
    */
  implicit class Ebesum(v: Vector[Double]){
    def +(v2: Vector[Double]): Vector[Double] = v.zip(v2).map{case(x1,x2)=>x1+x2}
  }

  implicit def ebesum(v1: Vector[Double],v2: Vector[Double]): Vector[Double] = v1 + v2

  def sd(x: Vector[Double]): Double = {
    val m = x.sum/x.size
    math.sqrt(x.map{case xx => (xx - m)*(xx - m)}.sum/x.size)
  }


}

