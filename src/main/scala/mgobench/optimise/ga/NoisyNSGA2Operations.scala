package mgobench.optimise.ga


import mgo._
import mgo.evolution.algorithm._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.ranking._
import tools._
import mgo.evolution.breeding._
import mgo.evolution.elitism._

import cats.data._
import cats.implicits._




object NoisyNSGA2Operations {

  def aggregated[I](fitness: I => Vector[Vector[Double]],
                    aggregation: Vector[Vector[Double]] => Vector[Double],
                    embedding: Vector[Vector[Double]] => Vector[Double] = (v: Vector[Vector[Double]]) => Vector(1.0 / v.size.toDouble)
                   )(i: I): Vector[Double] =
    // add additional objective function as an option
    //aggregation(fitness(i)) ++ Vector(1.0 / fitness(i).size.toDouble)
    aggregation(fitness(i)) ++ embedding(fitness(i))

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
                                                        embedding: Vector[Vector[Double]] => Vector[Double],
                                                        values: I => (Vector[Double], Vector[Int]),
                                                        historyAge: monocle.Lens[I, Long],
                                                        historySize: Int,
                                                        mu: Int): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, mergeHistories[M, I, Vector[Double]](historyAge, history)(historySize)) apply filterNaN(population, aggregated(history.get, aggregation))
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregated(history.get, aggregation,embedding)) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  }

}