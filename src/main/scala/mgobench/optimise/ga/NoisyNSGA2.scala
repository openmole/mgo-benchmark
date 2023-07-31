package mgobench.optimise.ga

import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution._
import mgo.evolution.algorithm._
import mgo.evolution.ranking._
import tools._
import breeding._
import elitism._
//import contexts._
import cats.data._
import cats.implicits._

import mgobench.optimise.Optimisation
import mgobench.optimise.ga.NSGA2.{NSGA2Instance, result, run}
import mgobench.problem.Problem
import mgobench.problem.coco.CocoProblem
import mgobench.result.Result

import mgobench.utils._

import scala.language.higherKinds




case class NoisyNSGA2(
                       lambda: Int,
                       mu: Int,
                       generations: Int,
                       historySize: Int = 100,

                     /**
                       * Reevaluation rate of each individual
                       */
                     cloneProbability: Double = 0.2,

                       /**
                         * Which embedding function to use
                         *  \in {inv-size, gaussian-ci}
                         */
                       embedding: String = "inv-size",

                     rng: scala.util.Random = new scala.util.Random
                     ) extends Optimisation {

  override def optimize(problem: Problem): Result = NoisyNSGA2.optimize(this,problem)

  override def name: String = "NSGA2-"+mu+"-"+lambda+"-"+generations+"-"+historySize+"-"+cloneProbability

}





object NoisyNSGA2 {


  def optimize(noisyNSGA2: NoisyNSGA2,problem: Problem): mgobench.result.Result = {
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
      * FIXME could be the maximal confidence interval ?
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

    import CDGenome._
    import NoisyIndividual._

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

    def initialGenomes[M[_] : cats.Monad : Random](lambda: Int, continuous: Vector[C]) =
      CDGenome.initialGenomes[M](lambda, continuous, Vector.empty)

    def breeding[M[_] : cats.Monad : Random : Generation](
                                                           lambda: Int,
                                                           cloneProbability: Double,
                                                           aggregation: Vector[Vector[Double]] => Vector[Double]): Breeding[M, Individual[Vector[Double]], Genome] =
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

    def expression(fitness: (util.Random, Vector[Double]) => Vector[Double], continuous: Vector[C]): (util.Random, Genome) => Individual[Vector[Double]] =
      NoisyIndividual.expression((rng, v, d) => fitness(rng, v), continuous)

    def elitism[M[_] : cats.Monad : Random : Generation](mu: Int,
                                                         historySize: Int,
                                                         aggregation: Vector[Vector[Double]] => Vector[Double],
                                                         embedding: Vector[Vector[Double]] => Vector[Double],
                                                         components: Vector[C]
                                                        ): Elitism[M, Individual[Vector[Double]]] =
      NoisyNSGA2Operations.elitism[M, Individual[Vector[Double]]](
        vectorFitness,
        aggregation,
        embedding,
        i => values(Individual.genome.get(i), components),
        Individual.historyAge,
        historySize,
        mu
      )

    def state[M[_] : cats.Monad : StartTime : Random : Generation] = mgo.algorithm.state[M, Unit](())

    def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)

    def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

    implicit def isAlgorithm[M[_] : Generation : Random : cats.Monad : StartTime]: Algorithm[NoisyNSGA2Instance, M, Individual, Genome, EvolutionState[Unit]] =
      new Algorithm[NoisyNSGA2Instance, M, Individual, Genome, EvolutionState[Unit]] {
      def initialPopulation(t: NoisyNSGA2Instance) =
        noisy.initialPopulation[M, Genome, Individual[Vector[Double]]](
          NoisyNSGA2.initialGenomes[M](t.lambda, t.continuous),
          NoisyNSGA2.expression(t.fitness, t.continuous))

      def step(t: NoisyNSGA2Instance): Kleisli[M, Vector[Individual[Vector[Double]]], Vector[Individual[Vector[Double]]]] =
        noisy.step[M, Individual[Vector[Double]], Genome](
          NoisyNSGA2.breeding[M](
            t.lambda,
            t.cloneProbability,
            t.aggregation),
          NoisyNSGA2.expression(t.fitness, t.continuous),
          NoisyNSGA2.elitism[M](
            t.mu,
            t.historySize,
            t.aggregation,
            t.embedding,
            t.continuous
          )
        )

      def state = NoisyNSGA2.state[M]
    }


}