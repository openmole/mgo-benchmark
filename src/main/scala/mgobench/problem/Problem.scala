package mgobench.problem

import mgo.evolution.C

trait Problem {

  def dimension: Int
  def number_of_objectives: Int
  //def number_of_constraints: Int
  def lower_bounds: Vector[Double]
  def upper_bounds: Vector[Double]

  def problemName: String

  /**
    * mgobench.optimize.Optimization boundaries
    */
  def boundaries: Vector[C] = {
    val bounds = lower_bounds zip upper_bounds
    bounds.map{b => C(b._1,b._2)}
  }

  /**
    * no evaluation
    * @param x
    * @return
    */
  def evaluateFunction(x : Vector[Double]) : Vector[Double] = fitness(x)


  def fitness: Vector[Double] => Vector[Double]

  def isEmpty: Boolean

  def evaluations: Int

  //override def toString: String = fitness.toString()
  override def toString: String = problemName

}


object Problem {

  /**
    * Empty problem
    */
  val emptyProblem = FitnessProblem(0,0,0,Vector.empty,Vector.empty,x => x,true)

}
