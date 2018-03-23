

//import scala.collection.immutable.Vector

import mgo._

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


  val emptyProblem : Problem = Problem(0,0,0,0,Vector.empty,Vector.empty,"empty","empty",0)

  /**
    * Constructs the problem from the pointer.
    * @param pointer pointer to the coco_problem_t object
    * @throws Exception
    */
  def apply(coco: CocoJNI,pointer: Long) : Problem = {
    if(pointer == 0) return emptyProblem
    else {
      return new Problem(
        pointer,
        coco.cocoProblemGetDimension(pointer),
        coco.cocoProblemGetNumberOfObjectives(pointer),
        coco.cocoProblemGetNumberOfConstraints(pointer),
        coco.cocoProblemGetSmallestValuesOfInterest(pointer).to[Vector],
        coco.cocoProblemGetLargestValuesOfInterest(pointer).to[Vector],
        coco.cocoProblemGetId(pointer),
        coco.cocoProblemGetName(pointer),
        coco.cocoProblemGetIndex(pointer)
      )
    }
  }


  /**
    * Evaluates the function in point x and returns the result as an array of doubles.
    * @param x
    * @return the result of the function evaluation in point x
    */
  def evaluateFunction(coco: CocoJNI, problem: Problem,x : Vector[Double]) : Vector[Double] = {
    //println(x.toArray.toString)
    //println(coco.cocoEvaluateFunction(problem.pointer, x.toArray).toString)
    coco.cocoEvaluateFunction(problem.pointer, x.toArray).to[Vector]
  }

  /**
    * Evaluates the constraint in point x and returns the result as an array of doubles.
    * @param x
    * @return the result of the constraint evaluation in point x
    */
  def evaluateConstraint(coco: CocoJNI,problem: Problem,x: Vector[Double]): Vector[Double] = {
    coco.cocoEvaluateConstraint(problem.pointer, x.toArray).to[Vector]
  }

  def getBoundaries(problem: Problem): Vector[C] = {
    val minvals = problem.lower_bounds
    val maxvals = problem.upper_bounds
    for {
      minval <- minvals
      maxval <- maxvals
    } yield C(minval,maxval)
  }


  /*
  def getLargestFValuesOfInterest(problem: Problem): Vector[Double] = {
    CocoJNI.cocoProblemGetLargestFValuesOfInterest(problem.pointer).to[Vector]
  }

  def getEvaluations(problem: Problem): Long = {
    CocoJNI.cocoProblemGetEvaluations(problem.pointer)
  }

  def getEvaluationsConstraints(problem: Problem): Long = {
    CocoJNI.cocoProblemGetEvaluationsConstraints(problem.pointer)
  }

  def isFinalTargetHit(problem: Problem): Boolean = {
    CocoJNI.cocoProblemIsFinalTargetHit(problem.pointer) == 1
  }
  */

}

