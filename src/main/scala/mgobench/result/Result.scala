package mgobench.result

import mgobench.problem.Problem


/**
  * mgobench.result.Result is a Pareto front
  */
//type mgobench.result.Result = Vector[(Vector[Double],Vector[Double])]

case class Result(
                  points: Vector[Vector[Double]],
                  values: Vector[Vector[Double]],
                  precisions : Vector[Vector[Double]],
                  runs: Int,
                  problem: Problem
                 ) {

  /**
    * Average precision on all objectives and all points in the Pareto front
    * @return
    */
  def precision : Double = precisions.map{case eps => eps.sum / eps.size}.sum / precisions.size

}



object Result {

  val empty = Result(Vector.empty,Vector.empty,Vector.empty,0,Problem.emptyProblem)

  /**
    * Get the Pareto front corresponding to a result
    * @param result
    * @return
    */
  def resultAsParetoFront(result: Result): Vector[(Vector[Double],Vector[Double])] = result.points zip result.values

  /**
    * Dummy result for a deterministic Pareto front
    * @param front
    * @return
    */
  def paretoFrontAsResult(front: Vector[(Vector[Double],Vector[Double])],problem: Problem) : Result =
    Result(front.map{_._1},front.map{_._2},Vector.fill(front.size)(Vector.fill(front(0)._1.size)(0.0)),0,problem)


}




