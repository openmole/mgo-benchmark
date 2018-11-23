package mgobench.optimize.ga


import cats.data.Kleisli
import mgo.algorithm.CDGenome._
import mgo.algorithm._
import mgo.{C, arrayToVectorLens, contexts}
import mgo.contexts._
import mgobench.optimize._
import mgobench.problem._
import mgobench.result._
import monocle.macros.Lenses

import scala.language.higherKinds


/**
  * RTEA algorithm
  *
  * Fieldsend, J. E., & Everson, R. M. (2015).
  * The rolling tide evolutionary algorithm: A multiobjective optimizer for noisy optimization problems.
  * IEEE Transactions on Evolutionary Computation, 19(1), 103-117.
  *
  * Matlab implementation : https://github.com/fieldsend/ieee_tec_2014_rtea/blob/master/RTEA.m
  *
  * @param initPopSize
  * @param maxPopSize
  * @param generations
  * @param rng
  */
case class RTEA(
  initPopSize: Int,
  maxPopSize: Int,
  generations: Int,
  rng: scala.util.Random = new scala.util.Random
) extends Optimization {

  override def optimize(problem: Problem): Result = RTEA.optimize(this,problem)

  override def name: String = "RTEA-"+initPopSize+"-"+maxPopSize
}



object RTEA {

  def optimize(RTEA: RTEA,problem: Problem): Result = Result.empty

  case class RTEAInstance(
                           initPopSize: Int,
                           maxPopSize: Int,
                           fitness:  Vector[Double] => Vector[Double],
                           continuous: Vector[C] = Vector.empty
                         )

  object RTEAIndividual {

    @Lenses case class Individual(
                                   genome: Genome,
                                   fitnesses: Array[Array[Double]]
                                 )

    def vectorFitness = Individual.fitnesses.composeLens(arrayToVectorLens)

    def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, Array(f.toArray))

    def expression(fitness: (Vector[Double], Vector[Int]) => Vector[Double], components: Vector[C],observationNoise: Double): Genome => Individual =
      deterministic.expression[Genome, Individual](
        values(_, components),
        buildIndividual(_,_),
        fitness)

  }

  import RTEAIndividual._

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C]) = CDGenome.initialGenomes[M](lambda, continuous, Vector.empty)

  def expression(fitness: Vector[Double] => Vector[Double], continuous: Vector[C]): (util.Random, Genome) => Individual =
    expression(d => fitness(d), continuous)


  def step[M[_]: cats.Monad: Random,Individual,Genome]: Kleisli[M,Vector[Individual],Vector[Individual]] = Kleisli {population => population}


  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)


  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[RTEAInstance, M, Individual, Genome, EvolutionState[Unit]] =
    new Algorithm[RTEAInstance, M, Individual, Genome, EvolutionState[Unit]] {
      override def initialPopulation(t: RTEAInstance) =
        deterministic.initialPopulation[M, Genome, Individual](
          initialGenomes[M](t.initPopSize, t.continuous),
          expression(t.fitness, t.continuous))
      override def step(t: RTEAInstance) =
        step[M, Individual, Genome]()
      override def state = RTEA.state[M]
    }



}
