
import mgo._


trait Problem {

  def dimension: Int
  def number_of_objectives: Int
  def number_of_constraints: Int
  def lower_bounds: Vector[Double]
  def upper_bounds: Vector[Double]


  /**
    * Optimization boundaries
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


  override def toString: String = fitness.toString()


}

