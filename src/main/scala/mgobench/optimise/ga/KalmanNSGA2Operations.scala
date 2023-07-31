package mgobench.optimise.ga

import mgo._
import mgo.evolution.algorithm._
import mgo.evolution.algorithm.CDGenome._
import mgo.evolution.algorithm.deterministic
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools._
import cats.implicits._

import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.dominance.nonStrictDominance
import monocle.macros._

import mgobench.utils.implicits._

object KalmanNSGA2Operations {

  /**
    * A specific KalmanIndividual including uncertainty of the solution
    */
  object KalmanIndividual {

    @Lenses case class Individual(
                                   genome: Genome,
                                   fitness: Array[Double],
                                   uncertainty: Array[Double],
                                   evaluations: Int
                                 )


    def vectorFitness = Individual.fitness.composeLens(arrayToVectorLens)
    def vectorUncertainty = Individual.uncertainty.composeLens(arrayToVectorLens)

    def buildIndividual(g: Genome, f: Vector[Double], observationNoise: Double) = Individual(g, f.toArray, Array.fill(f.size)(observationNoise),1)

    def expression(fitness: (Vector[Double], Vector[Int]) => Vector[Double], components: Vector[C],observationNoise: Double): Genome => Individual =
      deterministic.expression[Genome, Individual](
        values(_, components),
        buildIndividual(_,_,observationNoise),
        fitness)
  }


  /**
    * Breeding : includes clone probability
    */
  def breeding[M[_]: cats.Monad: Generation: Random, I, G](
                                                            fitness: I => Vector[Double],
                                                            uncertainty: I => Vector[Double],
                                                            genome: I => G,
                                                            genomeValues: G => Vector[Double],
                                                            buildGenome: Vector[Double] => G,
                                                            crossover: GACrossover[M],
                                                            mutation: GAMutation[M],
                                                            lambda: Int,
                                                            cloneProbability: Double
                                                          ): Breeding[M, I, G] = Breeding { population =>
    // FIXME implicit must be imported at this level
    import mgobench.utils.implicits._
    for {
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](i=>fitness(i)+uncertainty(i)) apply population
      breeding = applyOperators[M, I, Vector[Double]](crossover, mutation, tournament[M, I, (Lazy[Int], Lazy[Double])](ranks), genome andThen genomeValues) apply population
      offspring <- breeding repeat ((lambda + 1) / 2)
      offspringGenomes = offspring.flatMap {
        case (o1, o2) =>
          def gv1 = o1.map(tools.clamp(_))
          def gv2 = o2.map(tools.clamp(_))
          Vector(buildGenome(gv1), buildGenome(gv2))
      }
      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)

      // the selection for cloning is done here with an uncertainty-based tournament
      acceptableFitnesses: Vector[Double] = population.map(fitness).zip(population.map(uncertainty)).map{case(f,u)=>(f+u).sum/f.size}
      ranksPrioritary <- lexicoRanking[M,I](acceptableRanking[M,I](fitness,acceptableFitnesses),paretoRanking[M,I](i=>uncertainty(i).map{1/_})) apply population
      gs <- clonesReplace[M, I, G](cloneProbability, population, genome, tournament[M, I, (Lazy[Int], Lazy[Int])](ranksPrioritary)) apply sizedOffspringGenomes
    } yield gs
  }


  /**
    *
    * Specific clone strategy : this is where Kalman heuristic is included for updating uncertainty and fitness
    *
    * @param fitness lens to access fitness vector for individuals
    * @param uncertainty lens to access uncertainty for individuals
    * @param observationNoise
    * @tparam M
    * @tparam I
    * @return
    */
  def updateUncertainty[M[_]: cats.Monad, I](
                fitness: monocle.Lens[I,Vector[Double]],
                uncertainty: monocle.Lens[I,Vector[Double]],
                evaluations: monocle.Lens[I,Int]
                           )(
                observationNoise: Double
                          ): UncloneStrategy[M, I] =
    (clones: Vector[I]) =>
          clones.reduce { (i1, i2) =>
            val f1: Vector[Double] = fitness.get(i1)
            val f2: Vector[Double] = fitness.get(i2)
            val u1: Vector[Double] = uncertainty.get(i1)
            val u2: Vector[Double] = uncertainty.get(i2)
            val n1: Int = evaluations.get(i1)
            val n2: Int = evaluations.get(i2)

            def updateObservationNoise(v1: Double,v2: Double): Double = v1*v2 / (v1 + v2)

            def updateObservationFitness(g1: Double, g2: Double, v1: Double, v2: Double): Double =
              (v1,v2) match {
                case (v1,v2) if v1 < v2 => g1 + v1 * (g2 - g1) / (v1 + v2)
                case (v1,v2) if v1 > v2 => g2 + v2 * (g1 - g2) / (v1 + v2)
                case (v1,v2) if v1 == v2 => (g1 + g2) / 2
              }

            val u: Vector[Double] = u1.zip(u2).map{case (d1,d2) => updateObservationNoise(d1,d2)}
            val f: Vector[Double] = f1.zip(f2).zip(u1).zip(u2).map{case (((g1,g2),v1),v2) => updateObservationFitness(g1,g2,v1,v2)}

            fitness.set(f)(uncertainty.set(u)(evaluations.set(n1+n2)(i1)))
          }.pure[M]


  /**
    * Elitism: use the specific cloning strategy
    *
    * @param fitness
    * @param values
    * @param mu
    * @tparam M
    * @tparam I
    * @return
    */
  def elitism[M[_]: cats.Monad: Random: Generation, I](
                                                        fitness: monocle.Lens[I,Vector[Double]],
                                                        values: I => (Vector[Double], Vector[Int]),
                                                        mu: Int,
                                                        uncertainty: monocle.Lens[I,Vector[Double]],
                                                        evaluations: monocle.Lens[I,Int],
                                                        observationNoise: Double
                                                      ) = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, updateUncertainty[M, I](fitness,uncertainty,evaluations)(observationNoise)) apply GenomeVectorDouble.filterNaN(population, fitness.get)
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness.get) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  }









}