package benchmark

import mgo._

import util.Random
import algorithm._
import mgo.algorithm.CDGenome.DeterministicIndividual.{Individual, vectorFitness}
import mgo.algorithm.CDGenome.{DeterministicIndividual, buildGenome}

object RandomSearch {


  /*def optimize(fitness : Vector[Double] => Vector[Double])(genome : Vector[C])(nsearchs : Int)(rng : Random) : Vector[(Vector[Double],Vector[Double])] = (1 until nsearchs).map { n =>
      val xvec = Vector.fill(genome.size)(rng.nextDouble)
      (fitness(xvec),xvec)
    }.to[Vector].sortWith((x,y) => x._1(0) < y._1(0))
  */

  /**
    * Optimize a fitness
    * @param fitness fitness function
    * @param genome genome : parameter boundaries
    * @param nsearchs number of executions
    * @param rng random number generator
    * @return
    */
  def optimize(fitness : Vector[Double] => Vector[Double])(genome : Vector[C])(nsearchs: Int)(rng : Random) : Vector[(Vector[Double],Vector[Double])] = {
    val points = Vector.fill(nsearchs)(genome.map {c => (c.high - c.low)*rng.nextDouble() +  c.low})
    val front = paretoFront(points,fitness)
    front.map{i => (i.genome.continuousValues.to[Vector],i.fitness.to[Vector])}
  }

  /**
    * Optimize a problem
    * @param problem
    * @param nsearchs
    * @return
    */
  def optimize(problem: Problem)(nsearchs: Int): Vector[(Vector[Double],Vector[Double])]  = {
    optimize(problem.evaluateFunction(_))(Problem.getBoundaries(problem))(nsearchs)(new util.Random)
  }




  // testing
  //println(RandomSearch.optimize(Rastrigin.rastrigin.apply)(Rastrigin.rastrigin.genome(2))(1000)(new util.Random(0)))
  //println(RandomSearch.optimize(Rosenbrock.rosenbrock.apply)(Rosenbrock.rosenbrock.genome(2))(10000)(new util.Random(0)))
  //println(Rosenbrock.counter)



}


