package mgobench.optimise.ga

import mgo.evolution.algorithm.GenomeVectorDouble.*
import mgo.evolution.*
import mgo.evolution.algorithm.*
import mgo.evolution.ranking.*

import mgobench.optimise.Optimisation
import mgobench.optimise.ga.NSGA2.{NSGA2Instance, result, run}
import mgobench.problem.Problem
import mgobench.problem.coco.CocoProblem
import mgobench.result.Result

import mgobench.utils._


/**
 *
 * @param lambda
 * @param mu
 * @param generations
 * @param historySize
 * @param cloneProbability Reevaluation rate of each individual
 * @param embedding Which embedding function to use \in {inv-size, gaussian-ci}
 * @param rng
 */
case class NoisyNSGA2Optimisation(
                       lambda: Int,
                       mu: Int,
                       generations: Int,
                       historySize: Int = 100,
                       cloneProbability: Double = 0.2,
                       embedding: String = "inv-size",
                       rng: scala.util.Random = new scala.util.Random
                     ) extends Optimisation {

  override def optimise(problem: Problem): Result = NoisyNSGA2.optimise(this,problem)

  override def name: String = "NoisyNSGA2-"+mu+"-"+lambda+"-"+generations+"-"+historySize+"-"+cloneProbability

}





object NoisyNSGA2Optimisation {


  def optimise(noisyNSGA2: NoisyNSGA2Optimisation,problem: Problem): mgobench.result.Result = {
    val prevevals = problem.evaluations
    val instance = NoisyNSGA2Instance(noisyNSGA2,problem)
    def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = instance.until(afterGeneration(noisyNSGA2.generations)).
      //trace { (s, is) =>if(s.generation%1000==0) {println(s.generation)}}.
      evolution
    val (finalState,finalPopulation) = (run(noisyNSGA2.rng) { imp => import imp._ ; evolution[DSL].eval})
    val res : Vector[NoisyNSGA2.Result] = result(instance,finalPopulation)
    //println("Noisynsga2 - result : "+res)
    //println("Noisynsga2 - result.fitnesses : "+res.map{_.fitness(0)})
    val orderedRes = res.sortWith{case (r1,r2) => r1.fitness(0) < r2.fitness(0) } // put best result in first for 1D - FIXME one dimensional only for now
    mgobench.result.Result(
      //points = orderedRes.map{_.continuous},
      points = orderedRes.take(1).map{_.continuous},
      //values = orderedRes.map{_.fitness},
      values = orderedRes.take(1).map{_.fitness},
      runs = problem.evaluations - prevevals,
      problem = problem.asInstanceOf[CocoProblem],
      optimizer = noisyNSGA2
    )
  }



  case class NoisyNSGA2Instance(
                                 mu: Int,
                                 lambda: Int,
                                 fitness: (util.Random, Vector[Double]) => Vector[Double],
                                 aggregation: Vector[Vector[Double]] => Vector[Double],
                                 aggregationWithCI: Vector[Vector[Double]] => Vector[Double],
                                 embedding: Vector[Vector[Double]] => Vector[Double],
                                 continuous: Vector[C] = Vector.empty,
                                 historySize: Int = 100,
                                 cloneProbability: Double = 0.2
                               )

  object NoisyNSGA2Instance {

    /**
      * Embedding with the additional indicator as the average confidence interval
      *   -> could be the maximal confidence interval ?
      * @param v
      * @return
      */
    def embeddingCI(v: Vector[Vector[Double]]): Vector[Double] = {
      val n = v.size.toDouble
      Vector(v.map(sd).sum*1.96 / (n*math.sqrt(n)))
    }

    def apply(noisyNSGA2: NoisyNSGA2,problem: Problem): NoisyNSGA2Instance = NoisyNSGA2Instance(
      noisyNSGA2.mu,
      noisyNSGA2.lambda,
      fitness = (_,x)=>problem.fitness(x),
      aggregation = aggregation,
      aggregationWithCI = aggregationWithCI,
      embedding = noisyNSGA2.embedding match { //dirty ; cant use trait mixin as the top-level instance is a case class ?
        case "inv-size" =>  (v: Vector[Vector[Double]]) => Vector(1.0 / v.size.toDouble)
        case "gaussian-ci" => embeddingCI
      },
      problem.boundaries,
      noisyNSGA2.historySize,
      noisyNSGA2.cloneProbability
    )
  }


    case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int)

    def result(population: Vector[Individual[Vector[Double]]],
               aggregation: Vector[Vector[Double]] => Vector[Double],
               //embedding: Vector[Vector[Double]]=>Vector[Double],
               continuous: Vector[C]) =
      keepFirstFront(population, NoisyNSGA2Operations.aggregated(vectorFitness.get, aggregation)).map {
        i =>
          val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous)
          Result(c, d, f, r)
      }

    def result(nsga2: NoisyNSGA2Instance, population: Vector[Individual[Vector[Double]]]): Vector[Result] =
      result(population,
        //nsga2.aggregation, // FIXME add CI computation as an option
        nsga2.aggregationWithCI,
        nsga2.continuous
      )

  //def result[P: Manifest](nsga2: NoisyNSGA2[P], population: Vector[Individual[P]]): Vector[Result[P]] =
  //      result[P](population, nsga2.aggregation, nsga2.continuous, keepAll = false)


  /**
   * Redefine aggregated operation from NoistNSGA2Operations
   * Others operations stay similar
   *
   * @param fitness
   * @param aggregation
   * @param embedding
   * @param i
   * @tparam I
   * @return
   */
  def aggregated[I](fitness: I => Vector[Vector[Double]],
                    aggregation: Vector[Vector[Double]] => Vector[Double],
                    embedding: Vector[Vector[Double]] => Vector[Double] = (v: Vector[Vector[Double]]) => Vector(1.0 / v.size.toDouble)
                   )(i: I): Vector[Double] =
  // add additional objective function as an option
  //aggregation(fitness(i)) ++ Vector(1.0 / fitness(i).size.toDouble)
    aggregation(fitness(i)) ++ embedding(fitness(i))


  def fitness[P: Manifest](aggregation: Vector[P] => Vector[Double]): Individual[P] => Vector[Double] =
        aggregated[Individual[P], P](
          vectorPhenotype[P].get,
          aggregation,
          i => i.focus(_.phenotypeHistory).get.size.toDouble)(_)


  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[S, P: Manifest](
                                        lambda: Int,
                                        operatorExploration: Double,
                                        cloneProbability: Double,
                                        aggregation: Vector[P] => Vector[Double],
                                        discrete: Vector[D],
                                        reject: Option[Genome => Boolean]): Breeding[S, Individual[P], Genome] =
    NoisyNSGA2Operations.adaptiveBreeding[S, Individual[P], Genome, P](
      fitness(aggregation),
      Focus[Individual[P]](_.genome).get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      _ => 1,
      lambda,
      reject,
      operatorExploration,
      cloneProbability)

  def expression[P: Manifest](phenotype: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome) => Individual[P] =
    NoisyIndividual.expression[P](phenotype, continuous)

  def elitism[S, P: Manifest](mu: Int, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C]): Elitism[S, Individual[P]] = {
    def individualValues(i: Individual[P]) = values(i.focus(_.genome).get, components)

    NoisyNSGA2Operations.elitism[S, Individual[P], P](
      fitness[P](aggregation),
      individualValues,
      mergeHistories(individualValues, vectorPhenotype[P], Focus[Individual[P]](_.historyAge), historySize),
      mu)
  }

  def reject[P](pse: NoisyNSGA2[P]): Option[Genome => Boolean] = NSGA2.reject(pse.reject, pse.continuous)


  implicit def isAlgorithm[P: Manifest]: Algorithm[NoisyNSGA2[P], Individual[P], Genome, NSGA2State] = new Algorithm[NoisyNSGA2[P], Individual[P], Genome, NSGA2State] {
    def initialState(t: NoisyNSGA2[P], rng: scala.util.Random) = EvolutionState(s = ())

    def initialPopulation(t: NoisyNSGA2[P], rng: scala.util.Random) =
      noisy.initialPopulation[Genome, Individual[P]](
        NoisyNSGA2.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        NoisyNSGA2.expression[P](t.fitness, t.continuous),
        rng)

    def step(t: NoisyNSGA2[P]) =
      (s, population, rng) =>
        noisy.step[NSGA2State, Individual[P], Genome](
          NoisyNSGA2.adaptiveBreeding[NSGA2State, P](
            t.lambda,
            t.operatorExploration,
            t.cloneProbability,
            t.aggregation,
            t.discrete,
            reject(t)),
          NoisyNSGA2.expression(t.fitness, t.continuous),
          NoisyNSGA2.elitism[NSGA2State, P](
            t.mu,
            t.historySize,
            t.aggregation,
            t.continuous),
          Focus[NSGA2State](_.generation),
          Focus[NSGA2State](_.evaluated))(s, population, rng)

  }



}