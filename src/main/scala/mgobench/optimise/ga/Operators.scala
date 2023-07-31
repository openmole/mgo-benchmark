package mgobench.optimise.ga

import mgo.evolution.breeding._

object Operators {
  def crossover[S]: GACrossover[S] = sbxC[S](2.0)
  def mutation[S]: GAMutation[S] = gaussianMutation[S](mutationRate = _ => 0.1, sigma = 0.01)
}