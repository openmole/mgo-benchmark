package mgobench.optimize


import mgo._
import mgobench.utils.benchmark
import mgobench.problem.Problem
import mgobench.problem.coco.CocoProblem
import mgobench.result.Result
import org.apache.commons.math3.genetics.StoppingCondition
import mgobench.utils._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random




case class RandomSearch (
                          val nsearchs : Int,
                          val nrepets : Int,
                          val seed : Int,
                          val stoppingCondition: (Double,Double) => Boolean = {case (d1,d2)=>false}
                        ) extends Optimization {
  /**
    * A new random object is created at each optimization but with the fixed seed for reproducibility
    *
    * Note : for additive gaussian noise, keep nrepets at 1 and use theoretical estimation for number of runs and confidence intervals
    * (rq : selection of the best may be noisy ?)
    *
    * @param problem the problem to solve
    * @return
    */
  override def optimize(problem: Problem): Result = {
    /*val res = new ArrayBuffer[Result]
    (1 to (nsearchs / resultStep) by 1).foreach { _ =>
      val prevevals = problem.evaluations
      val rawres: Vector[(Vector[Double], Vector[Double])] = RandomSearch.optimize(problem)(resultStep)(nrepets)
      res.append(Result(rawres.map {
        _._1
      }, rawres.map {
        _._2
      }, problem.evaluations - prevevals, problem.asInstanceOf[CocoProblem], this)
      )
    }
    res.toSeq
    */

    val prevevals = problem.evaluations
    val rawres: Vector[(Vector[Double], Vector[Double])] = RandomSearch.optimize(problem)(resultStep)(nrepets)
    Result(rawres.map {_._1}, rawres.map {_._2}, problem.evaluations - prevevals, problem.asInstanceOf[CocoProblem], this)
  }

  override def name: String = "RS-"+nsearchs+"-"+nrepets


}



object RandomSearch {

  def apply(nsearchs: Int): RandomSearch = RandomSearch(nsearchs,1,0,{case (d1,d2)=>false})


  /**
    * Optimize a fitness
    * @param fitness fitness function
    * @param genome genome : parameter boundaries
    * @param nsearchs number of executions
    * @param rng random number generator
    * @return
    */
  def optimize(fitness : Vector[Double] => Vector[Double])(genome : Vector[C])(nsearchs: Int)(nrepets: Int)(rng : Random) : Vector[(Vector[Double],Vector[Double])] = {
    val points = Vector.fill(nsearchs)(genome.map {c => (c.high - c.low)*rng.nextDouble() +  c.low})
    val fitnessValues = points.map{x => (1 to nrepets by 1).map{_ => fitness(x)}.reduce{ebesum}.map{_/nrepets}}
    val front = benchmark.paretoFront(points,fitnessValues)
    front.map{i => (i.genome.continuousValues.to[Vector],i.fitness.to[Vector])}
  }

  /**
    * Optimize a problem
    * @param problem
    * @param nsearchs
    * @return
    */
  def optimize(problem: Problem)(nsearchs: Int)(nrepets: Int): Vector[(Vector[Double],Vector[Double])]  = {
    optimize(problem.evaluateFunction(_))(problem.boundaries)(nsearchs)(nrepets)(new util.Random)
  }




  // testing
  //println(mgobench.optimize.RandomSearch.optimize(Rastrigin.rastrigin.apply)(Rastrigin.rastrigin.genome(2))(1000)(new util.Random(0)))
  //println(mgobench.optimize.RandomSearch.optimize(Rosenbrock.rosenbrock.apply)(Rosenbrock.rosenbrock.genome(2))(10000)(new util.Random(0)))
  //println(Rosenbrock.counter)



}


