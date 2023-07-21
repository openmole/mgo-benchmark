package mgobench.optimize.ga

import mgo._
//import mgo.contexts._
import freedsl.dsl._
import NSGA2.{NSGA2Instance, Result}
import mgo.algorithm.CDGenome.DeterministicIndividual._
import mgo.algorithm.CDGenome.Genome
import mgo.algorithm.CDGenome.NoisyIndividual.Individual
import mgo.algorithm.{Algorithm, EvolutionState, deterministic}
import mgobench.result._

import scala.util.Random


object Test {

  object GaussianNoise {
    def apply(rng: util.Random)(mu: Double = 0.0, sigma: Double = 0.01)= (sigma * rng.nextGaussian()) + mu
  }


  def replication(seed: Int)(fitness: Vector[Double]=>Vector[Double],boundaries:Vector[C]): Vector[mgobench.result.Result] = {
    val rng = new util.Random(seed)

    import NSGA2._

    val nsga2 = NSGA2Instance(
      mu = 100,
      lambda = 20,
      //fitness = x => Vector(Rastrigin.rastrigin(x) + GaussianNoise(rng)(mu = 0.0,sigma = 0.0)),
      //continuous = Rastrigin.rastrigin.genome(2)
      //fitness = x => Vector(Rosenbrock.rosenbrock(x),Rastrigin.rastrigin(x)),
      //continuous = Rosenbrock.rosenbrock.genome(2)++Rastrigin.rastrigin.genome(2)
      fitness = fitness,
      continuous = boundaries
    )

    /*
    val noisyNsga2 = NoisyNSGA2(
      mu=20,lambda=20,
      fitness = (rng : scala.util.Random,x: Vector[Double]) => Vector(Rosenbrock.rosenbrock(x) + GaussianNoise(rng)(0.0,0.1)),
      continuous = Rosenbrock.rosenbrock.genome(2),
      aggregation = (p : Vector[Vector[Double]]) => p.map( i => i.sum / i.length)
    )
    */

    /*
    def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
      nsga2.
        until(afterGeneration(10000)).
        trace { (s, is) =>
          if(s.generation%1000==0) {
            println(s.generation)
            //println(NSGA2.result(nsga2, is).mkString("\n"))
          }
        }.
        evolution


    val (finalState, finalPopulation) =
      NSGA2.run(rng) { imp =>
        import imp._
        evolution[DSL].eval
      }

    //NSGA2.result(nsga2, finalPopulation).head.fitness.head
    //NSGA2.result(nsga2, finalPopulation).head.continuous
    //NSGA2.result(nsga2, finalPopulation)

    //implicit def nsga2ResultToResult(nr: NSGA2.Result): mgobench.result.Result = mgobench.result.Result.empty
    def nsga2ResultToResult(nr: NSGA2.Result): mgobench.result.Result = mgobench.result.Result.empty

    NSGA2.result(nsga2, finalPopulation).map(nsga2ResultToResult)


    Vector.empty
  }

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

  */
    Vector.empty
    }


}

