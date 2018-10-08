package mgobench.optimize


import mgo._

import mgobench.utils.benchmark
import mgobench.problem.Problem
import mgobench.result.Result

import scala.util.Random




case class RandomSearch (
                          val nsearchs : Int,
                          val nrepets : Int,
                          val seed : Int
                        ) extends Optimization {
  /**
    * A new random object is created at each optimization but with the fixed seed for reproducibility
    * @param problem the problem to solve
    * @return
    */
  override def optimize(problem: Problem): Result = {
    val fitness = problem.fitness
    val bounds = problem.boundaries
    Result.paretoFrontAsResult(RandomSearch.optimize(fitness)(bounds)(nsearchs)(new util.Random(seed)),problem)
  }



}



object RandomSearch {

  def apply(nsearchs: Int): RandomSearch = RandomSearch(nsearchs,1,0)


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
    val front = benchmark.paretoFront(points,fitness)
    front.map{i => (i.genome.continuousValues.to[Vector],i.fitness.to[Vector])}
  }

  /**
    * Optimize a problem
    * @param problem
    * @param nsearchs
    * @return
    */
  def optimize(problem: Problem)(nsearchs: Int): Vector[(Vector[Double],Vector[Double])]  = {
    optimize(problem.evaluateFunction(_))(problem.boundaries)(nsearchs)(new util.Random)
  }




  // testing
  //println(mgobench.optimize.RandomSearch.optimize(Rastrigin.rastrigin.apply)(Rastrigin.rastrigin.genome(2))(1000)(new util.Random(0)))
  //println(mgobench.optimize.RandomSearch.optimize(Rosenbrock.rosenbrock.apply)(Rosenbrock.rosenbrock.genome(2))(10000)(new util.Random(0)))
  //println(Rosenbrock.counter)



}


