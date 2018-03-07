
import mgo._
import mgo.contexts._
import freedsl.dsl._

object Ellipsoidal {


  object ellipsoidal {
    def apply(i: Vector[Double]): Double = {
      var k : Double = 0.0
      return (i.map(x => {
        k = k + 1; return (k * x * x).toDouble
      }).sum)
    }
    def genome(size: Int) = Vector.fill(size)(C(-10,10))
  }

}

