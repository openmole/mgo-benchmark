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
    * Benchmarks a set of optimizers on a suite (integrated set of problems)
    *
    * @param optimizers
    * @param suite
    * @return Everything flatten, store problem in Result
    */
  def benchmark(optimizers: Seq[Optimization], nBootstraps: Int, suite: Suite,problemsNumber: Int = 2): Seq[Result] = {
    var currentProblem = suite.getNextProblem
    val res = new ArrayBuffer[Result]
    var k = 0
    while(!currentProblem.isEmpty&&k<problemsNumber){
      (1 until nBootstraps).foreach {case i =>
        println("Solving problem " + currentProblem.toString + " with " + optimizers.size + " optimizers (" + optimizers.mkString(";")+" repet "+i)
        optimizers.foreach { case o => res.append(o.optimize(currentProblem)) }
      }
      currentProblem = suite.getNextProblem
      k=k+1
    }
    res.toSeq
  }


}
