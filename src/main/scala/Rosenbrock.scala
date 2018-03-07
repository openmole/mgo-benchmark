
import mgo._
import mgo.contexts._
import freedsl.dsl._

object Rosenbrock {

  object rosenbrock {
    def apply(i: Vector[Double]): Double =
      (1 - i(0))*(1 - i(0)) + 100*(i(1) - i(0)*i(0))*(i(1) - i(0)*i(0))
    def genome(size: Int) = Vector.fill(size)(C(-3,3))
  }


}