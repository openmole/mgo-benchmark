package mgobench.problem.coco

import mgobench.problem.Problem
import mgobench.utils.CocoJNI
import java.util.concurrent.locks.ReentrantLock

import mgobench.problem.noise.Noise

import scala.util.Random

case class CocoProblem (
                         pointer: Long, // Pointer to the coco_problem_t object
                         coco: CocoJNI,
                         dimension: Int,
                         number_of_objectives: Int,
                         //number_of_constraints: Int,
                         lower_bounds: Vector[Double],
                         upper_bounds: Vector[Double],
                         id: String,
                         fun: String,
                         instance: Int,
                         name: String,
                         index: Long,
                         isEmpty: Boolean
                       ) extends Problem {

  /**
    * evaluate the problem
    */
  override def evaluateFunction(x: Vector[Double]): Vector[Double] = fitness(x)

  override def fitness: Vector[Double] => Vector[Double] = {CocoProblem.evaluateFunction(coco,this)}

  override def problemName: String = name

  override def evaluations: Int = coco.cocoProblemGetEvaluations(pointer).toInt

}




object CocoProblem {


  val emptyProblem : CocoProblem = CocoProblem(0,null,0,0,Vector.empty,Vector.empty,"empty","empty",0,"empty",0,true)

  //val evaluationLock: ReentrantLock = new ReentrantLock

  /**
    * Constructs the problem from the pointer.
    * @param pointer pointer to the coco_problem_t object
    * @throws Exception
    */
  def apply(coco: CocoJNI,pointer: Long,extnoise: Noise = null) : CocoProblem = {
    if(pointer == 0) emptyProblem
    else {
      val dim = coco.cocoProblemGetDimension(pointer)
      val objs = coco.cocoProblemGetNumberOfObjectives(pointer)
      val low = coco.cocoProblemGetSmallestValuesOfInterest(pointer).toVector
      val high = coco.cocoProblemGetLargestValuesOfInterest(pointer).toVector
      val id = coco.cocoProblemGetId(pointer)
      val fun = id.split("_")(1).replace("0", "")
      val inst = id.split("_")(2).replace("i", "").toInt
      val name = coco.cocoProblemGetName(pointer)
      val index = coco.cocoProblemGetIndex(pointer)

      if(extnoise==null) {
        new CocoProblem(pointer, coco, dim, objs, low, high, id, fun, inst, name, index, false)
      }else {
        new CocoProblem(pointer, coco, dim, objs, low, high, id, fun, inst, name, index, false) with Noise {
          override def noise(x: Vector[Double]): Vector[Double] = extnoise.noise(x)
          override def noiseName: String = extnoise.noiseName
        }
      }
    }
  }



  /**
    * Evaluates the function in point x and returns the result as an array of doubles.
    * @param x
    * @return the result of the function evaluation in point x
    */
  def evaluateFunction(coco:CocoJNI,problem: CocoProblem)(x : Vector[Double]) : Vector[Double] = {
    //println(s"Evaluating problem ${problem.asInstanceOf[AnyRef].toString} at x = ${x}")
    //java.lang.Thread.sleep((new Random).nextInt(100))
    //this.synchronized {
      // Synchro DOES NOT WORK
      //println(x.toArray.toString)
      //println(coco.cocoEvaluateFunction(problem.pointer, x.toArray).toString)
      //  add locking here
      /*while(evaluationLock.isLocked) {
      println("waiting for lock");
      java.lang.Thread.sleep(100)
    }
    if (!evaluationLock.isLocked) {
      println(s"Locking ${evaluationLock.toString} locked ${evaluationLock.isLocked} at ${System.currentTimeMillis()}")
      evaluationLock.lock()
      println(s"locked ${evaluationLock.toString} ${evaluationLock.isLocked} at ${System.currentTimeMillis()}")
     */
      //println(System.currentTimeMillis())

    val res = coco.cocoEvaluateFunction(problem.pointer, x.toArray).toVector

      /*println("Unlocking " + evaluationLock.toString)
      evaluationLock.unlock()
      */
      res
      //  }
      //  Vector.empty
    //}
  }

  //override def evaluateFunction()


  /**
    * Evaluates the constraint in point x and returns the result as a vector of doubles.
    * @param x
    * @return the result of the constraint evaluation in point x
    */
  /*def evaluateConstraint(coco:CocoJNI,problem: CocoProblem,x: Vector[Double]): Vector[Double] = {
    coco.cocoEvaluateConstraint(problem.pointer, x.toArray).to[Vector]
  }*/



  /*
  def getGenome(problem: mgobench.problem.Problem): Genome = {
    getBoundaries(problem)
  }
  */


  /*
  // returns null pointers
  def getLargestFValuesOfInterest(problem: mgobench.problem.Problem): Vector[Double] = {
    val largestFvals = mgobench.problem.coco.Suite.coco.cocoProblemGetLargestFValuesOfInterest(problem.pointer)
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
    //evaluationLock.lock()
    //this.synchronized {
    val evals = coco.cocoProblemGetEvaluations(problem.pointer)
    //evaluationLock.unlock()
    evals

    //}
  }

  /*
  def getEvaluationsConstraints(problem: mgobench.problem.Problem): Long = {
    mgobench.utils.CocoJNI.cocoProblemGetEvaluationsConstraints(problem.pointer)
  }

  def isFinalTargetHit(problem: mgobench.problem.Problem): Boolean = {
    mgobench.utils.CocoJNI.cocoProblemIsFinalTargetHit(problem.pointer) == 1
  }
  */


}
