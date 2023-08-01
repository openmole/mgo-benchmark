package mgobench.result

import mgobench.optimise.{Optimisation, RandomSearch}
import mgobench.problem.Problem
import mgobench.problem.coco.CocoProblem


/**
  * mgobench.result.Result is a Pareto front
  */
//type mgobench.result.Result = Vector[(Vector[Double],Vector[Double])]

/**
 *
 * @param points points
 * @param values Fitness values
 * @param precisions Precisions
 * @param evaluations Number of evaluation for each point
 * @param runs Total number of function evaluations
 * @param problem Corresponding problem
 * @param optimizer Optimizer
 * @param id an id giving the problem and the optimizer
 */
case class Result(
                   points: Vector[Vector[Double]],
                   values: Vector[Vector[Double]],
                   precisions : Vector[Vector[Double]],
                   evaluations: Vector[Int],
                   runs: Int,
                   problem: Problem,
                   optimizer: Optimisation,
                   id: String
                 ) {

  /**
    * Average precision on all objectives and all points in the Pareto front
    * @return
    */
  def precision : Double = precisions.map{eps => eps.sum / eps.size}.sum / precisions.size

}



object Result {

  val empty: Result = Result(Vector.empty,Vector.empty,Vector.empty,Vector.empty,0,Problem.emptyProblem,new RandomSearch(0,0,0),"empty")


  /**
    * Constructor for coco problems
    * @param points points
    * @param values values
    * @param runs runs
    * @param problem problem
    * @param optimizer optimizer
    * @return
    */
  def apply(points: Vector[Vector[Double]],values: Vector[Vector[Double]],runs: Int,problem: Problem,optimizer: Optimisation): Result = {
    Result(
      points,
      values,
      Vector.empty,
      Vector.empty,
      runs,
      problem,
      optimizer,
      problem.asInstanceOf[CocoProblem].fun+"_"+problem.asInstanceOf[CocoProblem].instance+"_"+problem.dimension+"_"+optimizer.name
    )
  }

  def apply(points: Vector[Vector[Double]],
            values: Vector[Vector[Double]],
            precisions: Vector[Vector[Double]],
            evaluations: Vector[Int],
            runs: Int,problem: Problem,optimizer: Optimisation): Result = {
    Result(
      points,
      values,
      precisions,
      evaluations,
      runs,
      problem,
      optimizer,
      problem.asInstanceOf[CocoProblem].fun+"_"+problem.asInstanceOf[CocoProblem].instance+"_"+problem.dimension+"_"+optimizer.name
    )
  }


  /**
    * Get the Pareto front corresponding to a result
    * @param result result
    * @return
    */
  def resultAsParetoFront(result: Result): Vector[(Vector[Double],Vector[Double])] = result.points zip result.values

  /**
    * FIX : not interesting as Result is contextualized to an optimizer and problem
    *
    * Dummy result for a deterministic Pareto front
    * @return
    */
  /*def paretoFrontAsResult(front: Vector[(Vector[Double],Vector[Double])],problem: Problem) : Result =
    Result(front.map{_._1},front.map{_._2},Vector.fill(front.size)(Vector.fill(front(0)._1.size)(0.0)),0,problem)
  */

}




