
package mgobench.optimize.ga


import mgo._
import algorithm._
import ranking._
import tools._
import breeding._
import elitism._
import contexts._
import cats.data._
import cats.implicits._
import freedsl.dsl._
import freedsl.tool._
import mgobench.optimize.Optimization
import mgobench.optimize.ga.NSGA2.{NSGA2Instance, result, run}
import mgobench.problem.Problem
import mgobench.problem.coco.CocoProblem
import mgobench.result.Result
import monocle.macros.Lenses
import shapeless._

import scala.language.higherKinds


case class KalmanNSGA2(
                      lambda: Int,
                      mu: Int,
                      generations: Int,
                      historySize: Int = 100,
                      cloneProbability: Double = 0.2,
                      rng: scala.util.Random = new scala.util.Random

                      ) extends Optimization {

  override def optimize(problem: Problem): Result = KalmanNSGA2.optimize(this,problem)

  override def name: String = ""
}



object KalmanNSGA2 {

  def optimize(kalmanNSGA2: KalmanNSGA2,problem: Problem): mgobench.result.Result = Result.empty

  /*
  def optimize(kalmanNSGA2: KalmanNSGA2,problem: Problem): mgobench.result.Result = {
    val prevevals = problem.evaluations
    val instance = KalmanNSGA2Instance(kalmanNSGA2,problem)
    def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = instance.until(afterGeneration(kalmanNSGA2.generations)).
      //trace { (s, is) =>if(s.generation%1000==0) {println(s.generation)}}.
      evolution
    val (finalState,finalPopulation) = (run(kalmanNSGA2.rng) { imp => import imp._ ; evolution[DSL].eval})
    val res : Vector[KalmanNSGA2.Result] = result(instance,finalPopulation)
    val orderedRes = res.sortWith{case (r1,r2) => r1.fitness < r2.fitness } // put best result in first for 1D
    mgobench.result.Result(
      points = orderedRes.map{_.continuous},
      values = orderedRes.map{_.fitness},
      runs = problem.evaluations - prevevals,
      problem = problem.asInstanceOf[CocoProblem],
      optimizer = kalmanNSGA2
    )
  }



  case class KalmanNSGA2Instance(
                                 mu: Int,
                                 lambda: Int,
                                 fitness: (util.Random, Vector[Double]) => Vector[Double],
                                 aggregation: Vector[Vector[Double]] => Vector[Double],
                                 continuous: Vector[C] = Vector.empty,
                                 historySize: Int = 100,
                                 cloneProbability: Double = 0.2
                               )

  object KalmanNSGA2Instance {
    def apply(kalmanNSGA2: KalmanNSGA2,problem: Problem): KalmanNSGA2Instance = KalmanNSGA2Instance(
      kalmanNSGA2.mu,kalmanNSGA2.lambda,
      fitness = (_,x)=>problem.fitness(x),
      aggregation = _.sum,
      problem.boundaries,
      kalmanNSGA2.historySize,kalmanNSGA2.cloneProbability
    )
  }

  import CDGenome._

  //import NoisyIndividual._
  /**
    * recreate a specific individual here to include uncertainty
    */
  @Lenses case class KalmanIndividual(
              genome: Genome,
              historyAge: Long,
              fitnessHistory: Array[Array[Double]],
              estimatedFitness: Vector[Double],
              uncertainty: Vector[Double])
  def vectorFitness = KalmanIndividual.fitnessHistory composeLens array2ToVectorLens

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int)

  def updateUncertaintyAndFitness(i: KalmanIndividual, continuous: Vector[C]) = {
    (scaleContinuousValues(continuousValues.get(i.genome), continuous),
      KalmanIndividual.genome composeLens discreteValues get i,
      //aggregation(vectorFitness.get(i)),
      // fitness update equation : f = f_prior + P_pr * (g - f_pr) / (P_pr + R)
      KalmanIndividual.fitnessHistory.get(i).size)
  }

  def result(population: Vector[KalmanIndividual], aggregation: Vector[Vector[Double]] => Vector[Double], continuous: Vector[C]): Vector[KalmanNSGA2.Result] = {
    val aggregfitness = NoisyNSGA2Operations.aggregated(vectorFitness.get, aggregation)
    keepFirstFront(population, aggregfitness
    ).map {
      i: KalmanIndividual =>
        val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous)
        Result(c, d, f, r)
    }
  }

  def result(nsga2: KalmanNSGA2Instance, population: Vector[KalmanIndividual]): Vector[Result] =
    result(population, nsga2.aggregation, nsga2.continuous)

  def initialGenomes[M[_] : cats.Monad : Random](lambda: Int, continuous: Vector[C]) =
    CDGenome.initialGenomes[M](lambda, continuous, Vector.empty)

  def breeding[M[_] : cats.Monad : Random : Generation](
                                                         lambda: Int,
                                                         cloneProbability: Double,
                                                         aggregation: Vector[Vector[Double]] => Vector[Double]): Breeding[M, KalmanIndividual, Genome] =
    NoisyNSGA2Operations.breeding[M, KalmanIndividual, Genome](
      vectorFitness.get,
      aggregation,
      KalmanIndividual.genome.get,
      continuousValues.get,
      buildGenome(_, None, Vector.empty, None),
      Operators.crossover,
      Operators.mutation,
      lambda,
      cloneProbability)

  def expression(fitness: (util.Random, Vector[Double]) => Vector[Double], continuous: Vector[C]): (util.Random, Genome) => KalmanIndividual =
    NoisyIndividual.expression((rng, v, d) => fitness(rng, v), continuous)

  def elitism[M[_] : cats.Monad : Random : Generation](mu: Int, historySize: Int, aggregation: Vector[Vector[Double]] => Vector[Double], components: Vector[C]): Elitism[M, Individual] =
    NoisyNSGA2Operations.elitism[M, KalmanIndividual](
      vectorFitness,
      aggregation,
      i => values(KalmanIndividual.genome.get(i), components),
      KalmanIndividual.historyAge,
      historySize,
      mu)

  def state[M[_] : cats.Monad : StartTime : Random : Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)

  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_] : Generation : Random : cats.Monad : StartTime]: Algorithm[KalmanNSGA2Instance, M, KalmanIndividual, Genome, EvolutionState[Unit]] =
    new Algorithm[KalmanNSGA2Instance, M, KalmanIndividual, Genome, EvolutionState[Unit]] {
      def initialPopulation(t: KalmanNSGA2Instance) =
        noisy.initialPopulation[M, Genome, KalmanIndividual](
          NoisyNSGA2.initialGenomes[M](t.lambda, t.continuous),
          NoisyNSGA2.expression(t.fitness, t.continuous))

      def step(t: KalmanNSGA2Instance): Kleisli[M, Vector[KalmanIndividual], Vector[KalmanIndividual]] =
        noisy.step[M, KalmanIndividual, Genome](
          NoisyNSGA2.breeding[M](
            t.lambda,
            t.cloneProbability,
            t.aggregation),
          NoisyNSGA2.expression(t.fitness, t.continuous),
          NoisyNSGA2.elitism[M](
            t.mu,
            t.historySize,
            t.aggregation,
            t.continuous))

      def state = NoisyNSGA2.state[M]
    }

*/


}


