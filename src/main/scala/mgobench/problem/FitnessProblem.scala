package mgobench.problem

import mgo.C


case class FitnessProblem(
                           dimension: Int,
                           number_of_objectives: Int,
                           number_of_constraints: Int,
                           lower_bounds: Vector[Double],
                           upper_bounds: Vector[Double],
                           fitness: Vector[Double] => Vector[Double],
                           isEmpty: Boolean
                         ) extends Problem {
  override def evaluateFunction(x: Vector[Double]): Vector[Double] = fitness(x)
}



object FitnessProblem {

  def apply(
             fitness: Vector[Double] => Vector[Double],
             bounds: Vector[C]
           ): FitnessProblem = FitnessProblem(
        bounds.size,
        fitness(bounds.map{case c => (c.low + c.high)/2}).size,
        0,
        bounds.map{_.low},
        bounds.map{_.high},
        fitness,
        false
        )



}


