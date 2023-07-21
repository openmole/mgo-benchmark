package mgobench.utils

import mgo.evolution.algorithm.CDGenome.DeterministicIndividual.{Individual, vectorFitness}
import mgo.evolution.algorithm.CDGenome.{DeterministicIndividual, buildGenome}
import mgo.evolution.elitism.keepFirstFront


object Benchmark {


  /**
    * Get the Pareto front for a set of points
    * @param points
    * @param fitness
    * @return
    */
  def paretoFront(points : Vector[Vector[Double]],fitnessValues : Vector[Vector[Double]]): Vector[Individual] = {
    val population = points.zip(fitnessValues).map{case (x,f) => DeterministicIndividual.buildIndividual(buildGenome(x, None, Vector.empty, None), f)}
    //println(population)
    keepFirstFront(population, vectorFitness.get)
  }


}

