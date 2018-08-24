package mgobench.optimize.ga

import mgobench.optimize.Optimization
import mgobench.problem.Problem
import mgobench.result.Result

import mgo.algorithm.GenomeVectorDouble._
import mgo._
import algorithm._
import ranking._
import tools._
import breeding._
import elitism._
import contexts._

import cats.data._
import cats.implicits._

import freedsl.tool._
import shapeless._

import scala.language.higherKinds



object NSGA2 {

  import CDGenome._
  import DeterministicIndividual._

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C]) =
    CDGenome.initialGenomes[M](lambda, continuous, Vector.empty)

  def breeding[M[_]: Generation: Random: cats.Monad](lambda: Int): Breeding[M, Individual, Genome] =
    NSGA2Operations.breeding[M, Individual, Genome](
      vectorFitness.get,
      Individual.genome.get,
      continuousValues.get,
      buildGenome(_, None, Vector.empty, None),
      Operators.crossover,
      Operators.mutation,
      lambda)

  def expression(fitness: Vector[Double] => Vector[Double], components: Vector[C]): Genome => Individual =
    DeterministicIndividual.expression((v, d) => fitness(v), components)

  def elitism[M[_]: cats.Monad: Random: Generation](mu: Int, components: Vector[C]): Elitism[M, Individual] =
    NSGA2Operations.elitism[M, Individual](
      vectorFitness.get,
      i => values(Individual.genome.get(i), components),
      mu)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])



  def result(population: Vector[Individual], continuous: Vector[C]): Vector[Result] =
    keepFirstFront(population, vectorFitness.get).map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, i.fitness.toVector)
    }

  def result(nsga2: NSGA2, population: Vector[Individual]): Vector[Result] = result(population, nsga2.continuous)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NSGA2, M, Individual, Genome, EvolutionState[Unit]] =
    new Algorithm[NSGA2, M, Individual, Genome, EvolutionState[Unit]] {
      override def initialPopulation(t: NSGA2) =
        deterministic.initialPopulation[M, Genome, Individual](
          NSGA2.initialGenomes[M](t.lambda, t.continuous),
          NSGA2.expression(t.fitness, t.continuous))
      override def step(t: NSGA2) =
        deterministic.step[M, Individual, Genome](
          NSGA2.breeding[M](t.lambda),
          NSGA2.expression(t.fitness, t.continuous),
          NSGA2.elitism(t.mu, t.continuous))
      override def state = NSGA2.state[M]
    }

}

case class NSGA2(
                  mu: Int,
                  lambda: Int,
                  fitness: Vector[Double] => Vector[Double],
                  continuous: Vector[C] = Vector.empty) extends Optimization {

  override def optimize(problem: Problem): Result = Result.empty
}

