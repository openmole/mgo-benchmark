package mgobench.optimise.ga


import cats.data.Kleisli
import mgo.evolution.*
import mgo.evolution.algorithm.*
import mgo.evolution.algorithm.CDGenome.*
import mgo.evolution.algorithm.CDGenome.NoisyIndividual.*
import mgo.tools.execution.*
import mgobench.optimise.*
import mgobench.problem.*
import mgobench.result.*


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
case class RTEAOptimisation(
  initPopSize: Int,
  maxPopSize: Int,
  generations: Int,
  rng: scala.util.Random = new scala.util.Random
) extends Optimisation {

  override def optimise(problem: Problem): Result = RTEAOptimisation.optimise(this,problem)

  override def name: String = "RTEA-"+initPopSize+"-"+maxPopSize
}



object RTEAOptimisation {

  def optimise(RTEA: RTEAOptimisation,problem: Problem): Result = Result.empty

  case class RTEAInstance(
                           initPopSize: Int,
                           maxPopSize: Int,
                           fitness:  Vector[Double] => Vector[Double],
                           continuous: Vector[C] = Vector.empty
                         )

  object RTEAIndividual {

    type Individual = NoisyIndividual.Individual[Vector[Double]]
    /*

    !! Not needed -> use NoisyIndividual
    case class Individual(
                                   genome: Genome,
                                   fitnesses: Array[Array[Double]]
                                 )

    def vectorFitness = Individual.fitnesses.composeLens(arrayToVectorLens)
    def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, Array(f.toArray))

     */

    // deterministic expression? to be checked - changed to noisy for correct type
    // ???
    def expression(fitness: (Vector[Double], Vector[Int]) => Vector[Double],
                   components: Vector[C],
                   observationNoise: Double // not used
                  ): Genome => Individual =
      //deterministic.expression[Genome, Individual](values(_, components), buildIndividual(_,_), fitness)
      noisy.expression[Genome, Individual, Vector[Double]](
        values(_, continuous),
        buildIndividual[Vector[Double]])(fitness)

  }


  // not needed: use CDGenome.initialGenomes
  //def initialGenomes(lambda: Int, continuous: Vector[C], rng: scala.util.Random): Vector[Genome] = CDGenome.initialGenomes[M](lambda, continuous, Vector.empty)

  /*def expression(fitness: Vector[Double] => Vector[Double], continuous: Vector[C]): (util.Random, Genome) => Individual =
    expression(d => fitness(d), continuous)
*/
  def expression(fitness: Vector[Double] => Vector[Double], continuous: Vector[C]): Genome => RTEAIndividual.Individual =
    RTEAIndividual.expression(d => fitness(d._1), continuous, 0.0)



// TBD
//  def step(size: Int): (S, Vector[I]) //Kleisli[M,Vector[Individual],Vector[Individual]] = // Kleisli[M,Vector[Individual],Vector[Individual]] {
//    population =>
//      population.pure[M]
//  }
//    mgo.evolution.elitism.randomO(size)



  implicit def isAlgorithm: Algorithm[RTEAInstance, RTEAIndividual.Individual, Genome, EvolutionState[Unit]] =
    new Algorithm[RTEAInstance, RTEAIndividual.Individual, Genome, EvolutionState[Unit]] {
      override def initialState(t: RTEAInstance, rng: scala.util.Random) = EvolutionState(s = ())
      override def initialPopulation(t: RTEAInstance, rng: scala.util.Random) =
        deterministic.initialPopulation[Genome, RTEAIndividual.Individual](
          CDGenome.initialGenomes(t.initPopSize, t.continuous, Vector.empty, None, rng),
          expression(t.fitness, t.continuous))
      override def step(t: RTEAInstance) = {case (s, population, rng): (EvolutionState[Unit], Vector[RTEAIndividual.Individual], scala.util.Random) => (s, population) }
       // RTEA.step[EvolutionState[Unit], RTEAIndividual.Individual, Genome](t.initPopSize)
    }



}
