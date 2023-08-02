package mgobench.optimise.ga

import mgobench.optimise.Optimisation
import mgobench.problem.Problem
import mgobench.result.Result
import mgobench.utils._
import mgobench.utils.implicits._
import mgobench.problem.coco.CocoProblem

import mgo.evolution._
import mgo.evolution.algorithm._

/**
  * NSGA2 optimizer
  * @param lambda lambda
  * @param mu mu
  * @param nrepets Number of repets for static sampling
  * @param generations number of generations
  * @param aggregType how the static sampling aggregation is done \in {avg,avgCI}
  * @param rng rng
  */
case class NSGA2Optimisation(
                  lambda: Int,
                  mu: Int,
                  nrepets: Int,
                  generations: Int,
                  aggregType: String = "avg",
                  rng: scala.util.Random = new scala.util.Random
                ) extends Optimisation {

  override def optimise(problem: Problem): Result = NSGA2Optimisation.optimise(this,problem)

  override def name: String = "NSGA2-"+mu+"-"+lambda+"-"+nrepets+"-"+generations

}


object NSGA2Optimisation {

  def optimise(nsga2: NSGA2Optimisation,problem: Problem): mgobench.result.Result = {
    val prevevals = problem.evaluations
    val instance = NSGA2Instance(nsga2,problem)
    def evolution: RunAlgorithm[NSGA2, CDGenome.DeterministicIndividual.Individual[Vector[Double]], CDGenome.Genome, EvolutionState[Unit]] =
      instance.algo.until(afterGeneration(nsga2.generations))
      //trace { (s, is) =>if(s.generation%1000==0) {println(s.generation)}}
    val (finalState,finalPopulation): (EvolutionState[Unit], Vector[CDGenome.DeterministicIndividual.Individual[Vector[Double]]]) = evolution.eval(nsga2.rng)
    val res : Vector[NSGA2.Result[Vector[Double]]] = NSGA2.result(finalPopulation,instance.continuous)
    val orderedRes = res.sortWith{case (r1,r2) => r1.fitness(0) < r2.fitness(0) } // put best result in first for 1D - !!! 1D only
    mgobench.result.Result(
      points = orderedRes.take(1).map{_.continuous},
      values = orderedRes.take(1).map{_.fitness},
      runs = problem.evaluations - prevevals,
      problem = problem.asInstanceOf[CocoProblem],
      optimizer = nsga2
    )
  }


  /**
    * An instance of the algorithm for a given fitness
    */
  case class NSGA2Instance(
                            algo: mgo.evolution.algorithm.NSGA2,
                            problem: Problem
                          ) {

  }

  object NSGA2Instance {

    //def aggreg(x1: Vector[Double],x2: Vector[Double]): Vector[Double] = x1.zip(x2).map{case (x,y) => x+y}

    /**
      * Random aggregation is done in the instance fitness
     *
     *  !!! possible uncertainty is not taken into account, not "fair" with the adaptive noisy nsga2
     *   should provide that as an option ?
     * @param nsga2 nsga2 optim
      * @param problem problem
      * @return
      */
    def apply(nsga2: NSGA2Optimisation, problem: Problem): NSGA2Instance = {
      def aggregFitness: Vector[Double] => Vector[Double] = x => nsga2.aggregType match {
        case "avg" => aggregation((1 to nsga2.nrepets).toVector.map { _ => problem.fitness(x) })
        case "avgCI" => aggregationWithCI((1 to nsga2.nrepets).toVector.map { _ => problem.fitness(x) })
      }
      val algo = mgo.evolution.algorithm.NSGA2(nsga2.mu, nsga2.lambda, aggregFitness, problem.boundaries)
      NSGA2Instance(algo, problem)
    }
  }

}


