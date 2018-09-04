package mgobench

import mgobench.optimize.Optimization
import mgobench.problem.{Problem, Suite}
import mgobench.result.Result

import scala.collection.mutable.ArrayBuffer

object Benchmark {


  /**
    * Benchmarks a set of optimizers on a set of problems
    * @param optimizers
    * @param problems
    * @return
    */
  def benchmark(optimizers: Seq[Optimization], problems: Seq[Problem]): Seq[Seq[Result]] = {
    optimizers.map{case o => problems.map{o.optimize(_)}}
  }

  /**
    * Benchmqrks a set of optimizers on a suite (integrated set of problems)
    *
    * @param optimizers
    * @param suite
    * @return
    */
  def benchmark(optimizers: Seq[Optimization], suite: Suite): Seq[Seq[Result]] = {
    var currentProblem = suite.getNextProblem
    val res = new ArrayBuffer[Seq[Result]]
    while(!currentProblem.isEmpty){
      println("Solving problem "+currentProblem.toString+" with "+optimizers.size+" optimizers")
      val currentResults: Seq[Result] = optimizers.map{case o => o.optimize(currentProblem)}
      res.append(currentResults)
      currentProblem = suite.getNextProblem
    }
    res.toSeq
  }


}
