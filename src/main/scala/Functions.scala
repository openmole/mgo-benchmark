import mgo._

object Rosenbrock {

  var counter = 0

  object rosenbrock {
    def apply(i: Vector[Double]): Vector[Double] = {
      counter = counter + 1
      Vector((1 - i(0)) * (1 - i(0)) + 100 * (i(1) - i(0) * i(0)) * (i(1) - i(0) * i(0)),(1 - i(1)) * (1 - i(1)) + 100 * (i(0) - i(1) * i(1)) * (i(0) - i(1) * i(1)))
    }

    def genome(size: Int) = Vector.fill(size)(C(-3,3))
  }


}



object Rastrigin {

  object rastrigin {
    def apply(i: Vector[Double]): Double = {
      10 * i.size + i.map(x => (x * x) - 10 * math.cos(2 * math.Pi * x)).sum
    }

    def genome(size: Int) = Vector.fill(size)(C(-5.12, 5.12))
  }



}



object Ellipsoidal {

  /**
    * f(x) = \sum_i i x_i^2
    */
  object ellipsoidal {
    def apply(i: Vector[Double]): Double =
    //var k : Double = 0.0
      i.zipWithIndex.map{(x,i) => (i+1) * x * x}.sum
    def genome(size: Int) = Vector.fill(size)(C(-10,10))
  }

}




