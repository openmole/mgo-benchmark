package mgobench.problem.coco

import mgobench.problem.Problem
import mgobench.problem.noise.Noise


case class NoisyCocoProblem(
                             cocoProblem: CocoProblem,
                             noise: Noise
                           ) extends Problem {


  override def dimension: Int = cocoProblem.dimension
  override def number_of_objectives: Int = cocoProblem.number_of_objectives
  //override def number_of_constraints: Int = cocoProblem.number_of_constraints
  override def lower_bounds: Vector[Double] = cocoProblem.lower_bounds
  override def upper_bounds: Vector[Double] = cocoProblem.upper_bounds
  override def problemName: String = cocoProblem.problemName+"_"+noise.noiseName
  override def evaluations: Int = cocoProblem.evaluations
  override def isEmpty: Boolean = cocoProblem.isEmpty

  override def fitness: Vector[Double] => Vector[Double] = {
    x: Vector[Double] => cocoProblem.fitness(x).zip(noise.noise(x)).map{case (f,b)=> f+b}}

}

object NoisyCocoProblem {

}

