

import mgo._
import mgo.contexts._
import freedsl.dsl._

object Ackley {



  object ackley {
    def apply(i: Vector[Double]): Double = {
      math.exp(-1 / 5 * math.sqrt((i.map(x => x * x)).sum/i.size))
    }
    def genome(size: Int) = Vector.fill(size)(C(-10,10))
  }



}
