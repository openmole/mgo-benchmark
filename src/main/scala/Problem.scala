
package benchmark

import mgo._


trait Problem {

  def dimension: Int
  def number_of_objectives: Int
  def number_of_constraints: Int
  def lower_bounds: Vector[Double]
  def upper_bounds: Vector[Double]


  /**
    * Optimization boundaries
    * @param problem
    * @return
    */
  def getBoundaries(problem: Problem): Vector[C] = {
    val bounds = problem.lower_bounds zip problem.upper_bounds
    bounds.map{b => C(b._1,b._2)}
  }

  /**
    * no evaluation
    * @param x
    * @return
    */
  def evaluateFunction(x : Vector[Double]) : Vector[Double] = x

}


case class CocoProblem (
  pointer: Long, // Pointer to the coco_problem_t object
  dimension: Int,
  number_of_objectives: Int,
  number_of_constraints: Int,
  lower_bounds: Vector[Double],
  upper_bounds: Vector[Double],
  id: String,
  name: String,
  index: Long
) extends Problem




object CocoProblem {


  val emptyProblem : Problem = CocoProblem(0,0,0,0,Vector.empty,Vector.empty,"empty","empty",0)

  /**
    * Constructs the problem from the pointer.
    * @param pointer pointer to the coco_problem_t object
    * @throws Exception
    */
  def apply(coco: CocoJNI,pointer: Long) : Problem = {
    if(pointer == 0) return emptyProblem
    else {
      return new CocoProblem(
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
  def evaluateFunction(problem: CocoProblem)(x : Vector[Double]) : Vector[Double] = {
    //println(x.toArray.toString)
    //println(coco.cocoEvaluateFunction(problem.pointer, x.toArray).toString)
    Suite.coco.cocoEvaluateFunction(problem.pointer, x.toArray).to[Vector]
  }

  //override def evaluateFunction()


  /**
    * Evaluates the constraint in point x and returns the result as a vector of doubles.
    * @param x
    * @return the result of the constraint evaluation in point x
    */
  def evaluateConstraint(problem: CocoProblem,x: Vector[Double]): Vector[Double] = {
    Suite.coco.cocoEvaluateConstraint(problem.pointer, x.toArray).to[Vector]
  }



  /*
  def getGenome(problem: Problem): Genome = {
    getBoundaries(problem)
  }
  */


  /*
  // returns null pointers
  def getLargestFValuesOfInterest(problem: Problem): Vector[Double] = {
    val largestFvals = Suite.coco.cocoProblemGetLargestFValuesOfInterest(problem.pointer)
    println(largestFvals)
    largestFvals.to[Vector]
  }
  */

  /**
    * Number of evals
    * @param problem
    * @return
    */
  def getEvaluations(problem: CocoProblem): Long = {
    Suite.coco.cocoProblemGetEvaluations(problem.pointer)
  }

  /*
  def getEvaluationsConstraints(problem: Problem): Long = {
    CocoJNI.cocoProblemGetEvaluationsConstraints(problem.pointer)
  }

  def isFinalTargetHit(problem: Problem): Boolean = {
    CocoJNI.cocoProblemIsFinalTargetHit(problem.pointer) == 1
  }
  */


}

