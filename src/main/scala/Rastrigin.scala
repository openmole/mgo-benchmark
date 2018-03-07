
import mgo._
import mgo.contexts._
import freedsl.dsl._

object Rastrigin {



  object rastrigin {
    def apply(i: Vector[Double]): Double =
      10 * i.size + i.map(x => (x * x) - 10 * math.cos(2 * math.Pi * x)).sum

    def genome(size: Int) = Vector.fill(size)(C(-5.12, 5.12))
  }



}
