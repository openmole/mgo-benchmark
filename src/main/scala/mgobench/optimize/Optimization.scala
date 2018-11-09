package mgobench.optimize

import mgobench.problem.Problem
import mgobench.result.Result

/**
  * Optimization algorithm
  */
//type Optimization = (Fitness,Vector[C]) => mgobench.result.Result


trait Optimization {

  /**
    * Returns q seq of results corresponding to successive iterations
    *
    * @param problem
    * @return
    */
  def optimize(problem: Problem) : Result

  def name: String

  def resultStep: Int = 1000

}


