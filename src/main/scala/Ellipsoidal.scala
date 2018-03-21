
import mgo._
import mgo.contexts._
import freedsl.dsl._

object Ellipsoidal {


  object ellipsoidal {
    def apply(i: Vector[Double]): Double =
      //var k : Double = 0.0
      i.map(x => x * x).sum
    def genome(size: Int) = Vector.fill(size)(C(-10,10))
  }

}

