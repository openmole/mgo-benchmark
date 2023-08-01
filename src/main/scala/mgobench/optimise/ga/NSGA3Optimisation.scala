
package mgobench.optimise.ga

import mgobench.optimise.Optimisation
import mgobench.problem.Problem
import mgobench.result.Result
import mgobench.utils.aggregation
import mgobench.problem.coco.CocoProblem
import mgobench.utils.implicits._

import mgo.evolution._
import mgo.evolution.algorithm._
import mgo.evolution.algorithm.CDGenome.DeterministicIndividual.Individual
import mgo.evolution.algorithm.CDGenome.Genome

/**
* Fieldsend, J. E., Chugh, T., Allmendinger, R., & Miettinen, K. (2019). A Feature Rich Distance-Based Many-Objective Visualisable Test Problem Generator. => possible benchmark for many objectives ?
*  at https://github.com/fieldsend/DBMOPP_generator
*/

/**
 *
 * @param popSize size of the population, generally taken close to number of ref points
 * @param generations number of generations
 * @param referencePoints reference points
 * @param repetitions number of repets to be used in static sampling setting
 * @param aggregation aggregation
 * @param rng rng
 */
case class NSGA3Optimisation(
                  popSize: Int,
                  generations: Int,
                  referencePoints: Int,
                  repetitions: Int = 1,
                  aggregation: Vector[Vector[Double]] => Vector[Double] = aggregation ,
                  rng: scala.util.Random = new scala.util.Random
                 ) extends Optimisation {

  override def optimise(problem: Problem): Result = NSGA3Optimisation.optimise(this,problem)

  override def name: String = "NSGA3-"+popSize+"-"+referencePoints+"-"+repetitions+"-"+generations


}




object NSGA3Optimisation {

  def optimise(nsga3: NSGA3Optimisation,problem: Problem): mgobench.result.Result = {
    val prevevals = problem.evaluations
    val instance = NSGA3Instance(nsga3,problem)
    def evolution: RunAlgorithm[NSGA3, Individual[Vector[Double]], Genome, EvolutionState[Unit]] =
      instance.algo.until(afterGeneration(nsga3.generations))
      //trace { (s, is) =>if(s.generation%2==0) {println(s.generation)}}.

    val (finalState,finalPopulation): (EvolutionState[Unit], Vector[Individual[Vector[Double]]]) = evolution.eval(nsga3.rng)
    val res : Vector[NSGA3.Result[Vector[Double]]] = NSGA3.result(finalPopulation,instance.algo.continuous)
    val orderedRes = res.sortWith{case (r1,r2) => r1.fitness(0) < r2.fitness(0) } // put best result in first for 1D - 1D only
    mgobench.result.Result(
      points = orderedRes.take(1).map{_.continuous},
      values = orderedRes.take(1).map{_.fitness},
      runs = problem.evaluations - prevevals,
      problem = problem.asInstanceOf[CocoProblem],
      optimizer = nsga3
    )
  }


  /**
    * Instance of the algorithm on a given fitness
    * @param popSize
    * @param referencePoints
    * @param continuous
    * @param fitness
    */
  case class NSGA3Instance(
                            algo: NSGA3,
                            problem: Problem
                          )

  object NSGA3Instance {

    /**
      * Construct a deterministic nsga3 instance :
      *  - fitness is aggregated
      *  - reference points are computed here
      *
      * @param nsga3
      * @param problem
      * @return
      */
    def apply(nsga3: NSGA3Optimisation, problem: Problem): NSGA3Instance = {
      assert(problem.number_of_objectives>1,"nsga3 should not be used for monoobjective problems")

      val refPoints = NSGA3Operations.ReferencePoints(nsga3.referencePoints,problem.number_of_objectives)

      //println("Reference points for NSGA3 = "+refPoints)

      val algo: NSGA3 = NSGA3(
        nsga3.popSize, refPoints,
        {(x: Vector[Double]) => nsga3.aggregation((1 to nsga3.repetitions).toVector.map { _ => problem.fitness(x) })} ,
        problem.boundaries
      )

      NSGA3Instance(algo, problem)
    }
  }

}


