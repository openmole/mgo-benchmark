package mgobench.utils

import mgo.algorithm.CDGenome.DeterministicIndividual.{Individual, vectorFitness}
import mgo.algorithm.CDGenome.{DeterministicIndividual, buildGenome}
import mgo.algorithm.keepFirstFront


object benchmark {


  /**
    * Get the Pareto front for a set of points
    * @param points
    * @param fitness
    * @return
    */
  def paretoFront(points : Vector[Vector[Double]],fitness : Vector[Double]=>Vector[Double]): Vector[Individual] = {
    val population = points.map{x => DeterministicIndividual.buildIndividual(buildGenome(x, None, Vector.empty, None), fitness(x))}
    //println(population)
    keepFirstFront(population, vectorFitness.get)
  }


}

