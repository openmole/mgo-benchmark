
package mgobench.optimise.ga


import mgo.*
import mgo.evolution.*
import mgo.evolution.algorithm.CDGenome.*
import mgo.evolution.algorithm.GenomeVectorDouble
import mgo.evolution.elitism.{Elitism, keepHighestRanked}
import mgo.evolution.ranking.paretoRankingMinAndCrowdingDiversity
import mgobench.optimise.Optimisation
import mgobench.optimise.ga.NSGA2.{NSGA2Instance, result, run}
import mgobench.problem.Problem
import mgobench.problem.coco.CocoProblem
import mgobench.result.Result
//import monocle.macros.Lenses
import monocle.macros.GenLens



case class KalmanNSGA2Optimisation(
                      lambda: Int,
                      mu: Int,
                      generations: Int,
                      cloneProbability: Double = 0.2,
                      observationNoise: Double = 1.0,
                      rng: scala.util.Random = new scala.util.Random
                      ) extends Optimisation {

  override def optimise(problem: Problem): Result = KalmanNSGA2Optimisation.optimise(this,problem)

  override def name: String = "KalmanNSGA2-"+mu+"-"+lambda+"-"+cloneProbability+"-"+observationNoise
}



object KalmanNSGA2Optimisation {



  def optimise(kalmanNSGA2: KalmanNSGA2Optimisation,problem: Problem): mgobench.result.Result = {
    val prevevals = problem.evaluations
    val instance = KalmanNSGA2Instance(kalmanNSGA2,problem)

    def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = instance.until(afterGeneration(kalmanNSGA2.generations)).
      trace { (s, _) =>if(s.generation%1000==0) {println(s.generation)}}.
      evolution

    //def algo[M[_]: Generation: Random: cats.Monad: StartTime: IO] = instance.until(afterGeneration(kalmanNSGA2.generations/100)).
    //   trace { (s, _) =>if(s.generation%1000==0) {println(s.generation)}}
    //def algo[M[_]: Generation: Random: cats.Monad: StartTime: IO] =  afterGeneration(kalmanNSGA2.generations/100).run(instance)

    /*def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = for {
      initialPop <- algo.algo.initialPopulation(algo.t)
      finalPop <- algo.step.fold(initialPop)(algo.stopCondition.getOrElse(never[M, KalmanNSGA2Operations.KalmanIndividual.Individual]))
      s <- algo.algo.state
    } yield (s, finalPop)
    */

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

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random) =
    CDGenome.initialGenomes[M](lambda, continuous, Vector.empty)

  def breeding[M[_]: Generation: Random: cats.Monad](lambda: Int,cloneProbability: Double): Breeding[M, Individual, Genome] =
    KalmanNSGA2Operations.breeding[M, Individual, Genome](
      // the fitness for rank selection corresponds to the estimated fitness plus the uncertainty
      //i => vectorFitness.get(i).zip(vectorUncertainty.get(i)).map{case (f,u) => f+u},
      vectorFitness.get,
      vectorUncertainty.get,
      Individual.genome.get,
      continuousValues.get,
      buildGenome(_, None, Vector.empty, None),
      Operators.crossover,
      Operators.mutation,
      lambda,
      cloneProbability
    )


    /**
     * Breeding : includes clone probability
     */
    def breeding[M[_] : cats.Monad : Generation : Random, I, G](
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

      def fitness: Individual

      // implicit must be imported at this level
      import mgobench.utils.implicits._
      for {
        ranks <- paretoRankingMinAndCrowdingDiversity[M, I](i => fitness(i) + uncertainty(i)) apply population
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
        acceptableFitnesses: Vector[Double] = population.map(fitness).zip(population.map(uncertainty)).map { case (f, u) => (f + u).sum / f.size }
        ranksPrioritary <- lexicoRanking[M, I](acceptableRanking[M, I](fitness, acceptableFitnesses), paretoRanking[M, I](i => uncertainty(i).map {
          1 / _
        })) apply population
        gs <- clonesReplace[M, I, G](cloneProbability, population, genome, tournament[M, I, (Lazy[Int], Lazy[Int])](ranksPrioritary)) apply sizedOffspringGenomes
      } yield gs
    }


  def expression(fitness: Vector[Double] => Vector[Double], components: Vector[C],observationNoise: Double): Genome => Individual =
    KalmanIndividual.expression((v, d) => fitness(v), components,observationNoise)


  /**
   *
   * Specific clone strategy : this is where Kalman heuristic is included for updating uncertainty and fitness
   *
   * @param fitness     lens to access fitness vector for individuals
   * @param uncertainty lens to access uncertainty for individuals
   * @return
   */
  def updateUncertainty(
                        fitness: monocle.Lens[I, Vector[Double]],
                        uncertainty: monocle.Lens[I, Vector[Double]],
                        evaluations: monocle.Lens[I, Int]
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

        def updateObservationNoise(v1: Double, v2: Double): Double = v1 * v2 / (v1 + v2)

        def updateObservationFitness(g1: Double, g2: Double, v1: Double, v2: Double): Double =
          (v1, v2) match {
            case (v1, v2) if v1 < v2 => g1 + v1 * (g2 - g1) / (v1 + v2)
            case (v1, v2) if v1 > v2 => g2 + v2 * (g1 - g2) / (v1 + v2)
            case (v1, v2) if v1 == v2 => (g1 + g2) / 2
          }

        val u: Vector[Double] = u1.zip(u2).map { case (d1, d2) => updateObservationNoise(d1, d2) }
        val f: Vector[Double] = f1.zip(f2).zip(u1).zip(u2).map { case (((g1, g2), v1), v2) => updateObservationFitness(g1, g2, v1, v2) }

        fitness.set(f)(uncertainty.set(u)(evaluations.set(n1 + n2)(i1)))
      }.pure[M]


  /**
   * Elitism: use the specific cloning strategy
   */
  def elitism(
               mu: Int,
               observationNoise: Double,
               components: Vector[C]
             ): Elitism[EvolutionState[Unit], KalmanIndividual.Individual] = {
    def fitness: KalmanIndividual.Individual => Vector[Double] = {i => i.fitness}
    def uncertainty: KalmanIndividual.Individual => Vector[Double] = {i => i.uncertainty}
    def evaluations: KalmanIndividual.Individual => Int = {i => i.evaluations}
    def values: KalmanIndividual.Individual => (Vector[Double],Vector[Int]) = {i => CDGenome.values(i.focus(_.genome).get, components)}
    (s, population, candidates, rng)  =>
    for {
      cloneRemoved <- applyCloneStrategy(values, updateUncertainty(fitness, uncertainty, evaluations)(observationNoise)) apply GenomeVectorDouble.filterNaN(population, fitness.get)
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness.get) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  }



  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double],uncertainty: Vector[Double],evaluations: Int)

  def result(population: Vector[Individual], continuous: Vector[C]): Vector[Result] = {

    // FIXME add as an option the function to select final solution given fitnesses and uncertainties
    //println(population.map {_.evaluations})
    //println(population.map{_.fitness(0)})
    //println(population.map {case i => i.fitness(0)+i.uncertainty(0)})

    keepFirstFront(
      population.map { i => Individual(i.genome, i.fitness.zip(i.uncertainty).map { case (f, u) => f + u }, i.uncertainty, i.evaluations) },
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

  /**
   * A specific KalmanIndividual including uncertainty of the solution
   */
  object KalmanIndividual {

    case class Individual(
                           genome: Genome,
                           fitness: Vector[Double],
                           uncertainty: Vector[Double],
                           evaluations: Int
                         )


    def individualFitness: Individual => Vector[Double] = Focus[Individual](_.fitness)

    def individualUncertainty: Individual => Vector[Double] = Focus[Individual](_.uncertainty)

    def buildIndividual(g: Genome, f: Vector[Double], observationNoise: Double): Individual =
      Individual(g, f, Vector.fill(f.size)(observationNoise), 1)

    def expression(
                    fitness: (Vector[Double], Vector[Int]) => Vector[Double],
                    components: Vector[C],
                    observationNoise: Double
                  ): Genome => Individual =
      deterministic.expression[Genome, Vector[Double], Individual](
        values(_, components),
        buildIndividual(_, _, observationNoise),
        fitness
      )
  }




}


