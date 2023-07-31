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
  * @param lambda
  * @param mu
  * @param nrepets Number of repets for static sampling
  * @param generations number of generations
  * @param aggregType how the static sampling aggregation is done \in {avg,avgCI}
  * @param rng
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

  import CDGenome._
  import DeterministicIndividual._

  def optimise(nsga2: NSGA2Optimisation,problem: Problem): mgobench.result.Result = {
    val prevevals = problem.evaluations
    val instance = NSGA2Instance(nsga2,problem)
    def evolution: RunAlgorithm[NSGA2, CDGenome.DeterministicIndividual.Individual[Vector[Double]], CDGenome.Genome, EvolutionState[Unit]] =
      instance.algo.until(afterGeneration(nsga2.generations))
      //trace { (s, is) =>if(s.generation%1000==0) {println(s.generation)}}
    val (finalState,finalPopulation): (EvolutionState[Unit], Vector[CDGenome.DeterministicIndividual.Individual[Vector[Double]]]) = evolution.eval(nsga2.rng)
    val res : Vector[NSGA2.Result[Vector[Double]]] = result(finalPopulation,instance.continuous)
    val orderedRes = res.sortWith{case (r1,r2) => r1.fitness(0) < r2.fitness(0) } // put best result in first for 1D - FIXME 1D only
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
     * FIXME possible uncertainty is not taken into account, not "fair" with the adaptive noisy nsga2
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




  // not needed: same code as mgo
  /*def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome]  =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)
  */

   /*
  def breeding[M[_]: Generation: Random: cats.Monad](lambda: Int): Breeding[M, Individual[Vector[Double]], Genome] =
    NSGA2Operations.breeding[M, Individual, Genome](
      vectorFitness.get,
      Individual.genome.get,
      continuousValues.get,
      buildGenome(_, None, Vector.empty, None),
      Operators.crossover,
      Operators.mutation,
      lambda)
*/

   /*
  def expression(fitness: Vector[Double] => Vector[Double], components: Vector[C]): Genome => Individual[Vector[Double]] =
    DeterministicIndividual.expression((v, d) => fitness(v), components)
*/

   /*
  def elitism[M[_]: cats.Monad: Random: Generation](mu: Int, components: Vector[C]): Elitism[M, Individual[Vector[Double]]] =
    NSGA2Operations.elitism[M, Individual[Vector[Double]]](
      vectorFitness.get,
      i => values(Individual.genome.get(i), components),
      mu)
*/

  /*
  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])
  */

  /*
  def result(population: Vector[Individual[Vector[Double]]], continuous: Vector[C]): Vector[Result] =
    keepFirstFront(population, vectorFitness.get).map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, i.fitness.toVector)
    }
  */


  //def result(nsga2: NSGA2Instance, population: Vector[Individual[Vector[Double]]]): Vector[Result] = mgo.evolution.algorithm.NSGA2.result(population, nsga2.continuous)

  //def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())
  //def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  //def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  /*
  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NSGA2Instance, M, Individual, Genome, EvolutionState[Unit]] =
    new Algorithm[NSGA2Instance, M, Individual, Genome, EvolutionState[Unit]] {
      override def initialPopulation(t: NSGA2Instance) =
        deterministic.initialPopulation[M, Genome, Individual[Vector[Double]]](
          NSGA2.initialGenomes[M](t.lambda, t.continuous),
          NSGA2.expression(t.fitness, t.continuous))
      override def step(t: NSGA2Instance) =
        deterministic.step[M, Individual[Vector[Double]], Genome](
          NSGA2.breeding[M](t.lambda),
          NSGA2.expression(t.fitness, t.continuous),
          NSGA2.elitism(t.mu, t.continuous))
      override def state = NSGA2.state[M]
    }
   */

}


