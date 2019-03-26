package mgobench.optimize.ga

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
import mgo.dominance.{Dominance, nonStrictDominance}
import shapeless._

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds




object NSGA3Operations {


  /**
    * reference points may either automatically computed given a fixed number, or provided by the user
    */
  type References = Either[Int,Vector[Vector[Double]]]



  /**
    * NSGA3 breeding : next provisory population is of size 2*mu
    *  ; filtering is done in elitism using reference points
    *
    * @param fitness
    * @param genome
    * @param genomeValues
    * @param buildGenome
    * @param crossover
    * @param mutation
    * @param lambda
    * @tparam M
    * @tparam I
    * @tparam G
    * @return
    */
  def breeding[M[_]: cats.Monad: Generation: Random, I, G](
                                                            fitness: I => Vector[Double],
                                                            genome: I => G,
                                                            genomeValues: G => Vector[Double],
                                                            buildGenome: Vector[Double] => G,
                                                            crossover: GACrossover[M],
                                                            mutation: GAMutation[M],
                                                          ): Breeding[M, I, G] = Breeding { population =>


    //def buildGenomes(pop: Vector[(Vector[Double],Vector[Double])]]): Vector[G] =  pop
    def breeding = applyOperators[M, I, Vector[Double]](crossover, mutation,
      randomSelection[M,I],
      //Kleisli{population: Vector[I] => population(0).pure[M]},
      //tournament[M, I, (Lazy[Int], Lazy[Double])](ranks), // no tournament at this stage ?
      genome andThen genomeValues) apply population

    for {
      //ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
      //offspring <- breeding repeat population.size//((lambda + 1) / 2)
      offspring: M[Vector[(Vector[Double],Vector[Double])]] <- breeding repeat population.size
      offspringGenomes = offspring.flatMap {
        case (o1, o2) =>
          def gv1 = o1.map(tools.clamp(_))
          def gv2 = o2.map(tools.clamp(_))
          Vector(buildGenome(gv1), buildGenome(gv2))
      }
      //sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, offspringGenomes.size)
    } yield sizedOffspringGenomes
  }


  /**
    * The particularity of nsga3 is done in the elistism step
    *  - keep successive pareto fronts until having a population larger than the pop expected
    *  - remove the last front added
    *  - fill the remaining points with the reference points heuristic
    *
    *  Note : through normalization, ref points must be recomputed each time, even with user-defined points
    *   (option : number of points, taken within the objective simplex (tricky to compute ?) ; or user-defined)
    *
    * @param fitness
    * @param values
    * @param mu
    * @tparam M
    * @tparam I
    * @return
    */
  def elitism[M[_]: cats.Monad: Random: Generation, I](
          fitness: I => Vector[Double],
          values: I => (Vector[Double], Vector[Int]),
          references: References,
          mu: Int): Elitism[M,I] = Elitism[M, I] { population =>
    for {
      // still remove clone for deterministic nsga3
      cloneRemoved <- applyCloneStrategy(values, keepFirst[M, I]) apply filterNaN(population, fitness)

      //ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply cloneRemoved

      // use ranks to construct successive pareto fronts
      // ! pb : careful to not reevaluate fitness
      // + crowding diversity ?
      fronts = successiveFronts(cloneRemoved,fitness)

      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite

  }


  // pareto front extraction
  //def keepFirstFront[I](population: Vector[I], fitness: I => Vector[Double]) = {
  /*  val dominating = ranking.numberOfDominating(fitness, population)
    val minDominating = dominating.map(_.value).min
    (population zip dominating).filter { case (_, d) => d.value == minDominating }.map(_._1)
  }*/

  /**
    * Successive fronts by number of dominating points
    *  (grouping by number of dominating points gives successive fronts)
    *
    *  FIXME note : this could be in mgo ?
    *
    * @param population
    * @param fitness
    * @tparam I
    * @return fronts in order, with population and corresponding fitness values
    */
  def successiveFronts[I](population: Vector[I], fitness: I => Vector[Double]): Vector[(Vector[I],Vector[Vector[Double]])] = {
    // fitness evaluation is done here in number of dominating
    val (fitnesses,dominating) = numberOfDominatingAndFitnesses(fitness, population)
    population.zip(dominating).zip(fitnesses).groupBy{ case ((_,d),f) => d.value}.toVector.sortBy{_._1}.map{case (_,v) => (v.map{_._1._1},v.map{_._2})}
  }


  /**
    * redefine dominating function as also needs fitness values
    * @param fitness
    * @param values
    * @param dominance
    * @tparam I
    * @return
    */
  def numberOfDominatingAndFitnesses[I](fitness: I => Vector[Double], values: Vector[I], dominance: Dominance = nonStrictDominance): (Vector[Vector[Double]],Vector[Lazy[Int]]) = {
    val fitnesses = values.map(i => fitness(i))
    def ranks =
      fitnesses.zipWithIndex.map {
        case (v1, index1) =>
          def containsNaN = v1.exists(_.isNaN)
          def otherIndividuals = fitnesses.zipWithIndex.filter { case (_, index2) => index1 != index2 }
          def numberOfDominatingIndividual = otherIndividuals.count { case (v2, _) => dominance.isDominated(v1, v2) }
          shapeless.Lazy(if (containsNaN) Int.MaxValue else numberOfDominatingIndividual)
      }
    (fitnesses,ranks)
  }

  /**
    * extract elite using ref point heuristic
    * @param population
    * @param fitness
    * @param reference
    * @tparam I
    * @return
    */
  def eliteWithReference[I](population: Vector[I],
                            fitness: I => Vector[Double],
                            references: References,
                           // size of elite is by default pop size / 2 (doubling population in breeding)
                            mu: Vector[I] => Int = _.size / 2
                           ): Vector[I] = {
    val allfronts = successiveFronts(population,fitness)
    val fronts = allfronts.map{_._1}
    val fitnesses = allfronts.map{_._2}
    val allfitnesses = fitnesses.reduce{case (v1,v2) => v1++v2} // dirty flatten
    val targetSize = mu(population)

    // returns everything if not enough population (rq : this shouldnt happen)
    if (fronts.map{_.size}.sum < targetSize) fronts.flatten
    else {
      // else successive fronts
      val res = new ArrayBuffer[I]
      val cumsizes = new ArrayBuffer[Int];cumsizes.append(0)
      val cumpops = new ArrayBuffer[Vector[I]];cumpops.append(Vector.empty) // better to cache sucessive pops
      fronts.foreach { i =>
        if (res.size < targetSize) res.append(i: _*)
        cumsizes.append(cumsizes.last + i.size)
        cumpops.append(cumpops.last++i)
      }

      // return everything if good number
      if (res.size == targetSize) res.toVector
      else{
        // tricky part
        // needs last front to be added and remove it ; ! remove the first element of cumsizes
        val lastfrontindex = cumsizes.tail.zipWithIndex.find{case (d,_) =>  d > targetSize}.get._1
        val lastfront = cumpops.tail(lastfrontindex)
        val provpop: Vector[I] = cumpops.tail(lastfrontindex - 1)

        // next candidate points to be drawn in lastfront, given ref points
        // -> normalize here
        val (normfitnesses,normreferences) = normalize(allfitnesses,references)

        // niching in association to reference points ; selection according to it
        val additionalPoints = referenceNichingSelection(normfitnesses,normreferences,population)
        provpop++additionalPoints
      }
    }

  }

  /**
    * normalize objectives and compute normalized reference points
    * @param fitnesses
    * @param references
    * @return
    */
  def normalize(fitnesses: Vector[Vector[Double]],references: References): (Vector[Vector[Double]],Vector[Vector[Double]]) = {
     //val refpoints: Vector[Vector[Double]] = computeReferencePoints(references)
    ???
  }


  /**
    * Aggregate normalized fitnesses on reference points ; select on this.
    * @param normalizedFitnesses
    * @param normalizedReferences
    * @param population
    * @tparam I
    * @return
    */
  def referenceNichingSelection[I](
                                 normalizedFitnesses: Vector[Vector[Double]],
                                 normalizedReferences: Vector[Vector[Double]],
                                 population: Vector[I]
                               ): Vector[I] = ???







}

