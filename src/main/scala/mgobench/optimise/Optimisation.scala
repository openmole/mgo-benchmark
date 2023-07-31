package mgobench.optimise

import mgobench.problem.Problem
import mgobench.result.Result

/**
  * Optimization algorithm
  */
//type Optimization = (Fitness,Vector[C]) => mgobench.result.Result


trait Optimisation {

  /**
    * Returns q seq of results corresponding to successive iterations
    *
    * @param problem
    * @return
    */
  def optimise(problem: Problem) : Result

  def name: String

  //def resultStep: Int = 1000

}


