





case class CocoProblem (
                         pointer: Long, // Pointer to the coco_problem_t object
                         coco: CocoJNI,
                         dimension: Int,
                         number_of_objectives: Int,
                         number_of_constraints: Int,
                         lower_bounds: Vector[Double],
                         upper_bounds: Vector[Double],
                         id: String,
                         name: String,
                         index: Long
                       ) extends Problem {
  /**
    * evaluate the problem
    */
  override def evaluateFunction(x: Vector[Double]): Vector[Double] = CocoProblem.evaluateFunction(coco,this)(x)

}




object CocoProblem {


  val emptyProblem : CocoProblem = CocoProblem(0,null,0,0,0,Vector.empty,Vector.empty,"empty","empty",0)

  /**
    * Constructs the problem from the pointer.
    * @param pointer pointer to the coco_problem_t object
    * @throws Exception
    */
  def apply(coco: CocoJNI,pointer: Long) : CocoProblem = {
    if(pointer == 0) return emptyProblem
    else {
      return new CocoProblem(
        pointer,
        coco,
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
  def evaluateFunction(coco:CocoJNI,problem: CocoProblem)(x : Vector[Double]) : Vector[Double] = {
    //println(x.toArray.toString)
    //println(coco.cocoEvaluateFunction(problem.pointer, x.toArray).toString)
    coco.cocoEvaluateFunction(problem.pointer, x.toArray).to[Vector]
  }

  //override def evaluateFunction()


  /**
    * Evaluates the constraint in point x and returns the result as a vector of doubles.
    * @param x
    * @return the result of the constraint evaluation in point x
    */
  def evaluateConstraint(coco:CocoJNI,problem: CocoProblem,x: Vector[Double]): Vector[Double] = {
    coco.cocoEvaluateConstraint(problem.pointer, x.toArray).to[Vector]
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
  def getEvaluations(coco:CocoJNI,problem: CocoProblem): Long = {
    coco.cocoProblemGetEvaluations(problem.pointer)
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
