package mgobench

import mgobench.optimize.Optimization
import mgobench.problem.Problem
import mgobench.result.Result

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


}
