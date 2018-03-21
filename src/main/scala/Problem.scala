

import scala.collection.immutable.Vector

/**
  * The problem contains some basic properties of the coco_problem_t structure that can be accessed
  * through its getter functions.
  */
case class Problem (
  pointer: Long, // Pointer to the coco_problem_t object
  dimension: Int,
  number_of_objectives: Int,
  number_of_constraints: Int,
  lower_bounds: Vector[Double],
  upper_bounds: Vector[Double],
  id: String,
  name: String,
  index: Long
                   )


object Problem {


  /**
    * Constructs the problem from the pointer.
    * @param pointer pointer to the coco_problem_t object
    * @throws Exception
    */
  def apply(pointer: Long) = new Problem(
    pointer,
    CocoJNI.cocoProblemGetDimension(pointer),
    CocoJNI.cocoProblemGetNumberOfObjectives(pointer),
    CocoJNI.cocoProblemGetNumberOfConstraints(pointer),
    CocoJNI.cocoProblemGetSmallestValuesOfInterest(pointer),
    CocoJNI.cocoProblemGetLargestValuesOfInterest(pointer),
    CocoJNI.cocoProblemGetId(pointer),
    CocoJNI.cocoProblemGetName(pointer),CocoJNI.cocoProblemGetIndex(pointer)
  )



  /**
    * Evaluates the function in point x and returns the result as an array of doubles.
    * @param x
    * @return the result of the function evaluation in point x
    */
  def evaluateFunction(problem: Problem,x : Vector[Double]) : Vector[Double] = {
    CocoJNI.cocoEvaluateFunction(problem.pointer, x.toArray).to[Vector]
  }

  /**
    * Evaluates the constraint in point x and returns the result as an array of doubles.
    * @param x
    * @return the result of the constraint evaluation in point x
    */
  def evaluateConstraint(problem: Problem,x: Vector[Double]): Vector[Double] = {
    CocoJNI.cocoEvaluateConstraint(problem.pointer, x.toArray).to[Vector]
  }

  def getLargestFValuesOfInterest(problem: Problem): Vector[Double] = {
    CocoJNI.cocoProblemGetLargestFValuesOfInterest(problem.pointer).to[Vector]
  }

  def getEvaluations(problem: Problem): Long = {
    CocoJNI.cocoProblemGetEvaluations(pointer)
  }

  def getEvaluationsConstraints(problem: Problem): Long = {
    CocoJNI.cocoProblemGetEvaluationsConstraints(problem.pointer)
  }

  def isFinalTargetHit(problem: Problem): Boolean = {
    CocoJNI.cocoProblemIsFinalTargetHit(problem.pointer) == 1
  }

}
