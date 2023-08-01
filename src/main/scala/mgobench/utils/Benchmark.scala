package mgobench.utils

import mgo.evolution.algorithm.CDGenome.DeterministicIndividual.{Individual, individualFitness}
import mgo.evolution.algorithm.CDGenome.{DeterministicIndividual, buildGenome}
import mgo.evolution.elitism.keepFirstFront


object Benchmark {


  /**
    * Get the Pareto front for a set of points
    * @param points points
    * @param fitnessValues fitnesses
    * @return
    */
  def paretoFront(points : Vector[Vector[Double]],fitnessValues : Vector[Vector[Double]]): Vector[Individual[Vector[Double]]] = {
    val population = points.zip(fitnessValues).map{case (x,f) => DeterministicIndividual.buildIndividual(buildGenome(x, None, Vector.empty, None), f)}
    keepFirstFront(population, individualFitness[Vector[Double]](p => p))
  }


}

