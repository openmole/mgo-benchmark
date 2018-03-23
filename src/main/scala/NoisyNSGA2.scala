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



object NoisyNSGA2 {

  import CDGenome._
  import NoisyIndividual._

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int)

  def result(population: Vector[Individual], aggregation: Vector[Vector[Double]] => Vector[Double], continuous: Vector[C]) =
    keepFirstFront(population, NoisyNSGA2Operations.aggregated(vectorFitness.get, aggregation)).map {
      i =>
        val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous)
        Result(c, d, f, r)
    }

  def result(nsga2: NoisyNSGA2, population: Vector[Individual]): Vector[Result] =
    result(population, nsga2.aggregation, nsga2.continuous)

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C]) =
    CDGenome.initialGenomes[M](lambda, continuous, Vector.empty)

  def breeding[M[_]: cats.Monad: Random: Generation](
                                                      lambda: Int,
                                                      cloneProbability: Double,
                                                      aggregation: Vector[Vector[Double]] => Vector[Double]): Breeding[M, Individual, Genome] =
    NoisyNSGA2Operations.breeding[M, Individual, Genome](
      vectorFitness.get,
      aggregation,
      Individual.genome.get,
      continuousValues.get,
      buildGenome(_, None, Vector.empty, None),
      Operators.crossover,
      Operators.mutation,
      lambda,
      cloneProbability)

  def expression(fitness: (util.Random, Vector[Double]) => Vector[Double], continuous: Vector[C]): (util.Random, Genome) => Individual =
    NoisyIndividual.expression((rng, v, d) => fitness(rng, v), continuous)

  def elitism[M[_]: cats.Monad: Random: Generation](mu: Int, historySize: Int, aggregation: Vector[Vector[Double]] => Vector[Double], components: Vector[C]): Elitism[M, Individual] =
    NoisyNSGA2Operations.elitism[M, Individual](
      vectorFitness,
      aggregation,
      i => values(Individual.genome.get(i), components),
      Individual.historyAge,
      historySize,
      mu)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NoisyNSGA2, M, Individual, Genome, EvolutionState[Unit]] = new Algorithm[NoisyNSGA2, M, Individual, Genome, EvolutionState[Unit]] {
    def initialPopulation(t: NoisyNSGA2) =
      noisy.initialPopulation[M, Genome, Individual](
        NoisyNSGA2.initialGenomes[M](t.lambda, t.continuous),
        NoisyNSGA2.expression(t.fitness, t.continuous))

    def step(t: NoisyNSGA2): Kleisli[M, Vector[Individual], Vector[Individual]] =
      noisy.step[M, Individual, Genome](
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

}

case class NoisyNSGA2(
                       mu: Int,
                       lambda: Int,
                       fitness: (util.Random, Vector[Double]) => Vector[Double],
                       aggregation: Vector[Vector[Double]] => Vector[Double],
                       continuous: Vector[C] = Vector.empty,
                       historySize: Int = 100,
                       cloneProbability: Double = 0.2)

object NoisyNSGA2Operations {

  def aggregated[I](fitness: I => Vector[Vector[Double]], aggregation: Vector[Vector[Double]] => Vector[Double])(i: I): Vector[Double] =
    aggregation(fitness(i)) ++ Vector(1.0 / fitness(i).size.toDouble)

  def breeding[M[_]: cats.Monad: Random: Generation, I, G](
                                                            history: I => Vector[Vector[Double]],
                                                            aggregation: Vector[Vector[Double]] => Vector[Double],
                                                            genome: I => G,
                                                            genomeValues: G => Vector[Double],
                                                            buildGenome: Vector[Double] => G,
                                                            crossover: GACrossover[M],
                                                            mutation: GAMutation[M],
                                                            lambda: Int,
                                                            cloneProbability: Double): Breeding[M, I, G] =
    Breeding { population =>
     for {
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregated(history, aggregation)) apply population
      breeding = applyOperators[M, I, Vector[Double]](crossover, mutation, tournament[M, I, (Lazy[Int], Lazy[Double])](ranks), genome andThen genomeValues) apply population
      offspring <- breeding repeat ((lambda + 1) / 2)
      offspringGenomes = offspring.flatMap {
        case (o1, o2) =>
          def gv1 = o1.map(tools.clamp(_))
          def gv2 = o2.map(tools.clamp(_))
          Vector(buildGenome(gv1), buildGenome(gv2))
      }
      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
      gs <- clonesReplace[M, I, G](cloneProbability, population, genome, tournament(ranks)) apply sizedOffspringGenomes
    } yield gs
  }


  def elitism[M[_]: cats.Monad: Random: Generation, I](
                                                        history: monocle.Lens[I, Vector[Vector[Double]]],
                                                        aggregation: Vector[Vector[Double]] => Vector[Double],
                                                        values: I => (Vector[Double], Vector[Int]),
                                                        historyAge: monocle.Lens[I, Long],
                                                        historySize: Int,
                                                        mu: Int): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, mergeHistories[M, I, Vector[Double]](historyAge, history)(historySize)) apply filterNaN(population, aggregated(history.get, aggregation))
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregated(history.get, aggregation)) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  }

}