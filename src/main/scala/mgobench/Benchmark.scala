package mgobench

import mgobench.optimise.Optimisation
import mgobench.problem.coco.CocoProblem
import mgobench.problem.{Problem, Suite}
import mgobench.result.Result

import scala.collection.mutable.ArrayBuffer

object Benchmark {


  /**
    * Benchmarks a set of optimizers on a set of problems
    * @param optimizers optimizers
    * @param problems problems
    * @return
    */
  def benchmark(optimizers: Seq[Optimisation], problems: Seq[Problem]): Seq[Seq[Result]] = {
    optimizers.map{o => problems.map{o.optimise}}
  }

  /**
    * Benchmarks a set of optimizers on a suite (integrated set of problems)
    *
    * @param optimizers optimizers
    * @param suite problem suite
    * @return Everything flatten (problem and optimizer stored in Result)
    */
  def benchmark(optimizers: Seq[Optimisation], nBootstraps: Int, suite: Suite, problemsNumber: Int = 1, problemFilter: Problem=>Boolean = { _=>true}): Seq[Result] = {
    println("Launching benchmark on Optimizers "+optimizers.mkString(";"))
    var currentProblem = suite.getNextProblem
    val res = new ArrayBuffer[Result]
    var k = 0
    while(!currentProblem.isEmpty&&k<problemsNumber){
      if(problemFilter(currentProblem)) {
        (0 until nBootstraps).foreach { i =>
          println("Solving problem " + currentProblem.toString + " with " + optimizers.size + " optimizers (" + optimizers.mkString(";") + " repet " + i)
          optimizers.foreach { o => res.append(o.optimize(currentProblem)) }
        }
        k=k+1
      }
      //println("Number of evaluations : "+currentProblem.evaluations+" ; best fval = "+res.takeRight(nBootstraps*optimizers.length).map{_.values(0)(0)}.min)
      //val pb = currentProblem.asInstanceOf[CocoProblem]
      //println(pb.id+" : "+pb.coco.cocoProblemGetSmallestValuesOfInterest(pb.pointer).mkString(","))

      currentProblem = suite.getNextProblem
      if(currentProblem.isEmpty) println("No more problems to solve")
    }
    res.toSeq
  }


}
