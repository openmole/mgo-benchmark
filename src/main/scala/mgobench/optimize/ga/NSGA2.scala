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
import freedsl.dsl._
import freedsl.tool._
import mgobench.problem.coco.CocoProblem
import shapeless._

import scala.language.higherKinds
//import scala.util.Random


case class NSGA2(
                  mu: Int,
                  lambda: Int,
                  nrepets: Int,
                  generations: Int,
                  rng: scala.util.Random = new scala.util.Random
                ) extends Optimization {

  override def optimize(problem: Problem): Result = NSGA2.optimize(this,problem)

  override def name="NSGA2-"+mu+"-"+lambda+"-"+nrepets+"-"+generations

}


object NSGA2 {

  import CDGenome._
  import DeterministicIndividual._

  def optimize(nsga2: NSGA2,problem: Problem): mgobench.result.Result = {
    val prevevals = problem.evaluations
    val instance = NSGA2Instance(nsga2,problem)
    def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = instance.until(afterGeneration(nsga2.generations)).
      //trace { (s, is) =>if(s.generation%1000==0) {println(s.generation)}}.
      evolution
    val (finalState,finalPopulation) = (run(nsga2.rng) { imp => import imp._ ; evolution[DSL].eval})
    val res : Vector[NSGA2.Result] = result(finalPopulation,instance.continuous)
    val orderedRes = res.sortWith{case (r1,r2) => r1.fitness < r2.fitness } // put best result in first for 1D
    mgobench.result.Result(
      points = orderedRes.map{_.continuous},
      values = orderedRes.map{_.fitness},
      runs = problem.evaluations - prevevals,
      problem = problem.asInstanceOf[CocoProblem],
      optimizer = nsga2
    )
  }


  case class NSGA2Instance(
                            mu: Int,
                            lambda: Int,
                            continuous: Vector[C],
                            fitness: Vector[Double] => Vector[Double]
                          ) {

  }

  object NSGA2Instance {

    /**
      * Random aggregation is done in the instance fitness
      * @param nsga2
      * @param problem
      * @return
      */
    def apply(nsga2: NSGA2, problem: Problem): NSGA2Instance = {
      NSGA2Instance(
        nsga2.mu,nsga2.lambda,
        problem.boundaries,
        x => (1 to nsga2.nrepets).map{_=>problem.fitness(x)}.reduce{(f:(Vector[Double],Vector[Double])) => f match{case f : (Vector[Double],Vector[Double]) => f._1.zip(f._2).map{case x: (Double,Double)=>x._1+x._2}}}.map{_/nsga2.nrepets}
      )
    }
  }



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

  def result(nsga2: NSGA2Instance, population: Vector[Individual]): Vector[Result] = result(population, nsga2.continuous)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NSGA2Instance, M, Individual, Genome, EvolutionState[Unit]] =
    new Algorithm[NSGA2Instance, M, Individual, Genome, EvolutionState[Unit]] {
      override def initialPopulation(t: NSGA2Instance) =
        deterministic.initialPopulation[M, Genome, Individual](
          NSGA2.initialGenomes[M](t.lambda, t.continuous),
          NSGA2.expression(t.fitness, t.continuous))
      override def step(t: NSGA2Instance) =
        deterministic.step[M, Individual, Genome](
          NSGA2.breeding[M](t.lambda),
          NSGA2.expression(t.fitness, t.continuous),
          NSGA2.elitism(t.mu, t.continuous))
      override def state = NSGA2.state[M]
    }

}


