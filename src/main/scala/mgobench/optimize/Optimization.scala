package mgobench.optimize

import mgobench.problem.Problem
import mgobench.result.Result

/**
  * Optimization algorithm
  */
//type Optimization = (Fitness,Vector[C]) => mgobench.result.Result


trait Optimization {

  def optimize(problem: Problem) : Result

}


