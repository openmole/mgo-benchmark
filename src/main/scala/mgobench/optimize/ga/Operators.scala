package mgobench.optimize.ga

import mgo.evolution.breeding.{bga, sbxC}
import mgo.contexts.Random

object Operators {
  def crossover[M[_]: cats.Monad: Random] = sbxC[M](2.0)
  def mutation[M[_]: cats.Monad: Random] = bga[M](mutationRate = _ => 0.1, mutationRange = 0.01)
}