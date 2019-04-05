
package mgobench.problem

import mgo.C

case class FitnessSuite(fitnesses:Seq[(Vector[Double]=> Vector[Double],Vector[C])]) extends Suite {

  //var currentFitness: Vector[Double] => Vector[Double] = fitnesses(0)
  var currentFitnessIndex: Int = 0

  override def getNextProblem: Problem = {
    val res = fitnesses(currentFitnessIndex)
    currentFitnessIndex = currentFitnessIndex + 1
    FitnessProblem(res._1,res._2)
  }

  override def reinitialize: Suite = this

}


object FitnessSuite {

  def apply(fitness: Vector[Double]=> Vector[Double],bounds: Vector[C]): FitnessSuite = FitnessSuite(
    Seq((fitness,bounds))
  )

}