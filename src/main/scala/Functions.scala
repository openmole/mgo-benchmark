import mgo._

import scala.math._

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
    def apply(vec: Vector[Double]): Double = {
      //var k : Double = 0.0
      val zipped = vec.zipWithIndex
      zipped.map {c => (c._2 + 1.0) * c._1 * c._1 }.sum
    }

    def genome(size: Int) = Vector.fill(size)(C(-10,10))
  }

}



object Ackley {



  object ackley {
    def apply(i: Vector[Double]): Double = {
      math.exp(-1 / 5 * math.sqrt((i.map(x => x * x)).sum/i.size))
    }

    def genome(size: Int) = Vector.fill(size)(C(-10,10))
  }



}



object Griewank {

  object griewank {
    def apply(x: Vector[Double]): Double = {
      (1.0 / 4000.0) * x.map { x => pow(x, 2) }.sum - (x.zip(Stream from 1) map {c => cos(c._1 / sqrt(c._2))}).product + 1
    }

    def genome(size: Int) = Vector.fill(size)(C(-40,40))
  }

}



object Langermann {

  object langermann {

    def apply(x : Vector[Double]) : Double = (Vector(3.0, 5.0, 2.0, 1.0, 7.0) zip Vector(5.0, 2.0, 1.0, 4.0, 9.0)).map {
      case (a,c) => - c * cos(Pi * (pow(x(0) - a, 2) + pow(x(1) - a, 2))) / exp((pow(x(0) - a, 2) + pow(x(1) - a, 2)) / Pi)
    }.sum

    def genome()=Vector.fill(2)(C(0,10))
  }

}










