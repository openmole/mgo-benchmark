

import mgo._
import util.Random


object RandomSearch {


  def optimize(fitness : Vector[Double] => Vector[Double])(genome : Vector[C])(nsearchs : Int)(rng : Random) : Vector[Double] = {
    (1 until nsearchs).map { n =>
      fitness(Vector.fill(genome.size)(rng.nextDouble))
    }
  }


}


