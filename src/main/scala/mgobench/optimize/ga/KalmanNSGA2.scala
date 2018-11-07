
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
import mgo.algorithm.CDGenome._
import mgobench.optimize.Optimization
import mgobench.optimize.ga.KalmanNSGA2Operations.KalmanIndividual
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
                      cloneProbability: Double = 0.2,
                      observationNoise: Double = 1.0,
                      rng: scala.util.Random = new scala.util.Random
                      ) extends Optimization {

  override def optimize(problem: Problem): Result = KalmanNSGA2.optimize(this,problem)

  override def name: String = "KalmanNSGA2-"+mu+"-"+lambda+"-"+cloneProbability+"-"+observationNoise+"-"+generations
}



object KalmanNSGA2 {



  def optimize(kalmanNSGA2: KalmanNSGA2,problem: Problem): mgobench.result.Result = {
    val prevevals = problem.evaluations
    val instance = KalmanNSGA2Instance(kalmanNSGA2,problem)

    //def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = instance.until(afterGeneration(kalmanNSGA2.generations)).
      //trace { (s, _) =>if(s.generation%100==0) {println(s.generation)}}.
    //  evolution
    def algo[M[_]: Generation: Random: cats.Monad: StartTime: IO] = instance.until(afterGeneration(kalmanNSGA2.generations/100))
    //def algo[M[_]: Generation: Random: cats.Monad: StartTime: IO] =  afterGeneration(kalmanNSGA2.generations/100).run(instance)

    def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = for {
      initialPop <- algo.algo.initialPopulation(algo.t)
      finalPop <- algo.step.fold(initialPop)(algo.stopCondition.getOrElse(never[M, KalmanNSGA2Operations.KalmanIndividual.Individual]))
      s <- algo.algo.state
    } yield (s, finalPop)

    val runres = run(kalmanNSGA2.rng) {
      imp => import imp._
        /*(1 to 100 by 1).map{
          _ =>
          evolution[DSL].eval
        }*/
        //zipWithState(evolution[DSL]).eval
        evolution[DSL].eval
    }
    //val finalPopulation = runres.map{_._2}.takeRight(1)(0)
    //runres.foreach{case r => println(result(instance,r._2).sortWith{case (r1,r2) => r1.fitness < r2.fitness })}
    val finalPopulation = runres._2

    val res : Vector[KalmanNSGA2.Result] = result(instance,finalPopulation)
    val orderedRes = res.sortWith{case (r1,r2) => r1.fitness < r2.fitness } // put best result in first for 1D
    mgobench.result.Result(
      points = orderedRes.map{_.continuous},
      values = orderedRes.map{_.fitness},
      precisions = orderedRes.map{_.uncertainty},
      evaluations = orderedRes.map{_.evaluations},
      runs = problem.evaluations - prevevals,
      problem = problem.asInstanceOf[CocoProblem],
      optimizer = kalmanNSGA2
    )
  }


  /**
    * Case class to instantiate one algorithm
    * @param mu
    * @param lambda
    * @param fitness
    * @param continuous
    * @param historySize
    * @param cloneProbability
    * @param observationNoise
    */
  case class KalmanNSGA2Instance(
                                 mu: Int,
                                 lambda: Int,
                                 fitness:  Vector[Double] => Vector[Double],
                                 continuous: Vector[C] = Vector.empty,
                                 cloneProbability: Double = 0.5,
                                 observationNoise: Double = 1.0
                               )

  object KalmanNSGA2Instance {
    def apply(kalmanNSGA2: KalmanNSGA2,problem: Problem): KalmanNSGA2Instance = KalmanNSGA2Instance(
      kalmanNSGA2.mu,kalmanNSGA2.lambda,
      fitness = x=>problem.fitness(x),
      problem.boundaries,
      kalmanNSGA2.cloneProbability,
      kalmanNSGA2.observationNoise
    )
  }

  import KalmanNSGA2Operations.KalmanIndividual._

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C]) =
    CDGenome.initialGenomes[M](lambda, continuous, Vector.empty)

  def breeding[M[_]: Generation: Random: cats.Monad](lambda: Int,cloneProbability: Double): Breeding[M, Individual, Genome] =
    KalmanNSGA2Operations.breeding[M, Individual, Genome](
      vectorFitness.get,
      Individual.genome.get,
      continuousValues.get,
      buildGenome(_, None, Vector.empty, None),
      Operators.crossover,
      Operators.mutation,
      lambda,
      cloneProbability
    )

  def expression(fitness: Vector[Double] => Vector[Double], components: Vector[C],observationNoise: Double): Genome => Individual =
    KalmanIndividual.expression((v, d) => fitness(v), components,observationNoise)

  def elitism[M[_]: cats.Monad: Random: Generation](mu: Int, observationNoise: Double, components: Vector[C]): Elitism[M, Individual] =
    KalmanNSGA2Operations.elitism[M, Individual](
      vectorFitness,
      i => values(Individual.genome.get(i), components),
      mu,
      vectorUncertainty,
      Individual.evaluations,
      observationNoise
    )

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double],uncertainty: Vector[Double],evaluations: Int)

  def result(population: Vector[Individual], continuous: Vector[C]): Vector[Result] = {

    // FIXME add as an option the function to select final solution given fitnesses and uncertainties
    println(population.map {_.evaluations})
    println(population.map{_.fitness(0)})
    println(population.map {case i => i.fitness(0)+i.uncertainty(0)})

    keepFirstFront(
      population.map { case i => Individual(i.genome, i.fitness.zip(i.uncertainty).map { case (f, u) => f + u }, i.uncertainty, i.evaluations) },
      vectorFitness.get).map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous),
        Individual.genome composeLens discreteValues get i,
        i.fitness.toVector,
        i.uncertainty.toVector,
        i.evaluations
      )
    }
  }

  def result(kalmanNSGA2Instance: KalmanNSGA2Instance, population: Vector[Individual]): Vector[Result] = result(population, kalmanNSGA2Instance.continuous)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  def zipWithState[M[_]: cats.Monad: StartTime: Random: Generation, T](op: M[T]): M[(EvolutionState[Unit], T)] = {
    import cats.implicits._
    for {
      t ← op
      newState ← KalmanNSGA2.state[M]
    } yield (newState, t)
  }

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[KalmanNSGA2Instance, M, Individual, Genome, EvolutionState[Unit]] =
    new Algorithm[KalmanNSGA2Instance, M, Individual, Genome, EvolutionState[Unit]] {
      override def initialPopulation(t: KalmanNSGA2Instance) =
        deterministic.initialPopulation[M, Genome, Individual](
          KalmanNSGA2.initialGenomes[M](t.lambda, t.continuous),
          KalmanNSGA2.expression(t.fitness, t.continuous, t.observationNoise))
      override def step(t: KalmanNSGA2Instance) =
        deterministic.step[M, Individual, Genome](
          KalmanNSGA2.breeding[M](t.lambda,t.cloneProbability),
          KalmanNSGA2.expression(t.fitness, t.continuous, t.observationNoise),
          KalmanNSGA2.elitism(t.mu,t.observationNoise, t.continuous))
      override def state = KalmanNSGA2.state[M]
    }



}


