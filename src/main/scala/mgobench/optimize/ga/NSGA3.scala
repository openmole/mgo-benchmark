
package mgobench.optimize.ga

import mgobench.optimize.Optimization
import mgobench.optimize.ga.NSGA3Operations._
import mgobench.problem.Problem
import mgobench.result.Result
import mgobench.utils.aggregation
import mgo._
import mgo.algorithm._
import mgo.ranking._
import mgo.breeding._
import mgo.elitism._
import mgo.contexts._
import cats.data._
import cats.implicits._
import freedsl.dsl._
import mgobench.problem.coco.CocoProblem


case class NSGA3 (
                   /**
                     * size of the population, generally taken close to number of ref points
                     */
                   popSize: Int,

                   /**
                     * number of generations
                     */

                   generations: Int,

                   /**
                     * reference points
                     */
                   referencePoints: References = AutoReferences(1),

                   /**
                     * number of repets to be used in static sampling setting
                     */
                   repetitions: Int = 1,

                   /**
                     * aggregation
                     */
                   aggregation: Vector[Vector[Double]] => Vector[Double] = aggregation ,

                   /**
                     * rng
                     */
                   rng: scala.util.Random = new scala.util.Random
                 ) extends Optimization {

  override def optimize(problem: Problem): Result = NSGA3.optimize(this,problem)

  override def name="NSGA3-"+popSize+"-"+References.number_of_points(referencePoints)+"-"+repetitions+"-"+generations


}




object NSGA3 {

  import CDGenome._
  import DeterministicIndividual._

  def optimize(nsga3: NSGA3,problem: Problem): mgobench.result.Result = {
    val prevevals = problem.evaluations
    val instance = NSGA3Instance(nsga3,problem)
    def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = instance.until(afterGeneration(nsga3.generations)).
      //trace { (s, is) =>if(s.generation%1000==0) {println(s.generation)}}.
      evolution
    val (finalState,finalPopulation) = (run(nsga3.rng) { imp => import imp._ ; evolution[DSL].eval})
    val res : Vector[NSGA3.Result] = result(finalPopulation,instance.continuous)
    val orderedRes = res.sortWith{case (r1,r2) => r1.fitness(0) < r2.fitness(0) } // put best result in first for 1D - FIXME 1D only
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
                            popSize: Int,
                            referencePoints: ReferencePoints,
                            continuous: Vector[C],
                            fitness: Vector[Double] => Vector[Double]
                          )

  object NSGA3Instance {

    /**
      * Construct a deterministic nsga3 instance :
      *  - fitness is aggregated
      *  - reference points are computed here
      *
      * @param nsga2
      * @param problem
      * @return
      */
    def apply(nsga3: NSGA3, problem: Problem): NSGA3Instance = {
      NSGA3Instance(
        nsga3.popSize,
        References.computeReferences(nsga3.referencePoints,problem.dimension),
        problem.boundaries,
        x => nsga3.aggregation((1 to nsga3.repetitions).toVector.map{_=>problem.fitness(x)})
      )
    }
  }



  def initialGenomes[M[_]: cats.Monad: Random](populationSize: Int, continuous: Vector[C]) =
    CDGenome.initialGenomes[M](populationSize, continuous, Vector.empty)

  def breeding[M[_]: Generation: Random: cats.Monad]: Breeding[M, Individual, Genome] =
    NSGA3Operations.breeding[M, Individual, Genome](
      vectorFitness.get,
      Individual.genome.get,
      continuousValues.get,
      buildGenome(_, None, Vector.empty, None),
      Operators.crossover,
      Operators.mutation)

  def expression(fitness: Vector[Double] => Vector[Double], components: Vector[C]): Genome => Individual =
    DeterministicIndividual.expression((v, _) => fitness(v), components)

  def elitism[M[_]: cats.Monad: Random: Generation](mu: Int,references: References, components: Vector[C]): Elitism[M, Individual] =
    NSGA3Operations.elitism[M, Individual](
      vectorFitness.get,
      i => values(Individual.genome.get(i), components),
      references,
      mu
    )

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])



  def result(population: Vector[Individual], continuous: Vector[C]): Vector[Result] =
    //keepFirstFront(population, vectorFitness.get).map { i =>
    population.map{ i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, i.fitness.toVector)
    }

  def result(nsga3: NSGA3Instance, population: Vector[Individual]): Vector[Result] = result(population, nsga3.continuous)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NSGA3Instance, M, Individual, Genome, EvolutionState[Unit]] =
    new Algorithm[NSGA3Instance, M, Individual, Genome, EvolutionState[Unit]] {
      override def initialPopulation(t: NSGA3Instance) =
        deterministic.initialPopulation[M, Genome, Individual](
          NSGA3.initialGenomes[M](t.popSize, t.continuous),
          NSGA3.expression(t.fitness, t.continuous))
      override def step(t: NSGA3Instance) =
        deterministic.step[M, Individual, Genome](
          NSGA3.breeding[M],
          NSGA3.expression(t.fitness, t.continuous),
          NSGA3.elitism(t.popSize,t.referencePoints, t.continuous))
      override def state = NSGA3.state[M]
    }

}


