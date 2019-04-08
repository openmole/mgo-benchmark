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
import freedsl.random.Random
import freedsl.tool._
import mgo.dominance.{Dominance, nonStrictDominance}
import shapeless._
import org.apache.commons.math3.linear._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds




object NSGA3Operations {


  /**
    * reference points may either automatically computed given a fixed number, or provided by the user
    *  -> computation if needed done once and for all at initialization
    */
  //type References = Either[Int,Vector[Vector[Double]]]
  // type References = (Vector[Vector[Double]],ReferenceType)
  sealed trait References{
    def references: Vector[Vector[Double]]
  }
  object References{

    def number_of_points(r: References): Int = r match {
      case ReferencePoints(references,_) => references.length;
      case AutoReferences(_,_) => 0 // FIXME depends on dimension !
    }

    def computeReferences(r:References,dimension: Int): ReferencePoints = r match {
      case ReferencePoints(references,_) => ReferencePoints(references)
      case AutoReferences(divisions,_) => ReferencePoints(simplexRefPoints(divisions,dimension),true)
    }

  }
  case class ReferencePoints(override val references: Vector[Vector[Double]],normalized: Boolean=false) extends References


  //case class AutoReferences(divisions: Int, references: Vector[Vector[Double]]) extends References
  // auto ref can be computed only when the problem (hence the dimension) is known)
  case class AutoReferences(divisions: Int,override val references: Vector[Vector[Double]]=Vector.empty) extends References

  object AutoReferences {
    //def apply(divisions: Int,dimension: Int): AutoReferences = AutoReferences(divisions,simplexRefPoints(divisions,dimension))

  }


  /**
    * compute auto ref points on the simplex
    *   (called at initialization)
    * @param divisions number of segments on each simplex bord line
    * @param dimension dimension of the space
    * @return
    */
  def simplexRefPoints(divisions: Int,dimension: Int): Vector[Vector[Double]] = {

    println("Computing simplex reference points with "+divisions+" divisions in dimension "+dimension)

    // this returns h ~ n for now (basis vector) -> implement systematic distribution using number of divisions
    //Vector.tabulate(dimension){i => Vector.tabulate(dimension){j => if (j==i) 1 else 0}}

    // BRUTE FORCE ALGO - surely exists much better
    val basis = Vector.tabulate(dimension){i => Vector.tabulate(dimension){j => if (j==i) 1.0 else 0.0}}

    def linePoint(ei: Vector[Double],ej: Vector[Double],ek: Vector[Double],k: Double,l: Double): Vector[Double] = {
      import mgobench.utils.implicits._
      if(k==0.0) ei else {
        val o = ei + (k * (ej - ei))
        val d = ei + (k * (ek - ei))
        //val n = (o - d).norm // error no need to renormalize
        //o + ((l * n / k) * (o - d))
        o + ((l / k) * (o - d))
      }
    }

    case class Point(point: Vector[Double])
    def uniquePoints(x: Vector[Vector[Double]]):Vector[Vector[Double]] = {
      x.map(Point(_)).distinct.map(_.point)
    }

    // FIXME check algo p=2 ≠ p>=3 ?
    uniquePoints(for {
      ei <- basis
      ej <- basis.filter(!_.equals(ei))
      ek <- basis
      k <- 0.0 to divisions.toDouble//(divisions.toDouble - 1.0) by 1/divisions.toDouble
      l <- 0.0 to k by 1.0
    } yield {
      linePoint(ei,ej,ek,k,l)
    })
  }


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

    println("breeding - pop size "+population.size)
    for {
      //ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
      //offspring <- breeding repeat population.size//((lambda + 1) / 2)
      //offspring: M[Vector[(Vector[Double],Vector[Double])]] <- breeding repeat population.size
      offspring <- breeding repeat population.size
      offspringGenomes = offspring.flatMap {
        case (o1, o2) =>
          def gv1 = o1.map(tools.clamp(_))
          //def gv2 = o2.map(tools.clamp(_))
          //Vector(buildGenome(gv1), buildGenome(gv2))
          Vector(buildGenome(gv1))
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
    println("elitism - pop size "+population.size)
    for {
      // still remove clone for deterministic nsga3
      cloneRemoved <- applyCloneStrategy(values, keepFirst[M, I]) apply filterNaN(population, fitness)
      //fronts = successiveFronts(cloneRemoved,fitness) // computed in eliteWithReference
      elite = eliteWithReference[M,I](cloneRemoved,fitness, references)
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
    *
    *
    * @param population
    * @param fitness
    * @tparam I
    * @return fronts in order, with population and corresponding fitness values
    */
  // FIXME note : this could be in mgo ?
  // FIXME 2 : number of dominating DOES NOT exactly correspond to number of front ?
  def successiveFrontsApproxNumberofDominating[I](population: Vector[I], fitness: I => Vector[Double]): Vector[(Vector[I],Vector[Vector[Double]])] = {
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
    * Exact successive fronts computation
    * @param population
    * @param fitness
    * @tparam I
    * @return Vector of fronts, coded by (individuals: Vector[I],
    *         fitnesses in same order: Vector[Vector[Double]],
    *         indices in initial population: Vector[Int])
    */
  // FIXME front num are Lazy[Int] ?
  def successiveFronts[I](population: Vector[I], fitness: I => Vector[Double]): Vector[(Vector[I],Vector[Vector[Double]],Vector[Int])] = {
    // evaluate all fitness and put in map so that function are not reevaluated at each front computation
    println("successive pareto fronts")
    val fitnesses = population.map(i => fitness(i))
    val fitnessmap = population.zip(fitnesses).toMap
    def compfitness: I => Vector[Double] = i => fitnessmap(i)
    //val frontnums = new mutable.HashMap[I,Lazy[Int]]
    val frontnums = new mutable.HashMap[I,Int]
    var currentPop = population;var currentFrontNum = 0
    while(frontnums.keySet.size < population.size){
      //println(currentFrontNum+" - "+frontnums.keySet.size+"/"+population.size)
      val currentFront = keepFirstFront(currentPop,compfitness)
      //println(ranking.numberOfDominating(compfitness, currentPop))
      currentPop = currentPop.filter(i => !currentFront.contains(i))
      // FIXME putting a Lazy here does not work (no increment)
      //currentFront.foreach(i => frontnums.put(i,shapeless.Lazy(currentFrontNum)))
      currentFront.foreach(i => frontnums.put(i,currentFrontNum))
      currentFrontNum = currentFrontNum + 1
    }
    //frontnums.toMap.zip(fitnesses).zipWithIndex.groupBy{ case (((_,d),f),j) => d.value}.toVector.sortBy{_._1}.map{case (_,v) => (v.map{_._1._1._1}.toVector,v.map{_._1._2}.toVector,v.map{_._2}.toVector)}
    frontnums.toMap.zip(fitnesses).zipWithIndex.groupBy{ case (((_,d),f),j) => d}.toVector.sortBy{_._1}.map{case (_,v) => (v.map{_._1._1._1}.toVector,v.map{_._1._2}.toVector,v.map{_._2}.toVector)}
  }

  /**
    * extract elite using ref point heuristic
    * @param population
    * @param fitness
    * @param reference
    * @tparam I
    * @return
    */
  def eliteWithReference[M[_]: cats.Monad: Random,I](population: Vector[I],
                            fitness: I => Vector[Double],
                            references: References,
                           // size of elite is by default pop size / 2 (doubling population in breeding)
                            mu: Vector[I] => Int = {(i: Vector[I]) => i.size / 2 }
                           ): Vector[I] = {
    println("elite with ref - pop size "+population.size)
    val allfronts = successiveFronts(population,fitness)
    println("number of pareto fronts = "+allfronts.size)
    val fronts = allfronts.map{_._1}
    println("front sizes = "+fronts.map{_.size})
    val fitnesses = allfronts.map{_._2}
    val frontindices = allfronts.map{_._3}
    val allfitnesses = fitnesses.reduce{_++_}//{(v:(Vector[Vector[Double]],Vector[Vector[Double]])) => v._1++v._2} // dirty flatten
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

      //println("cumulated front sizes : "+cumsizes)

      // return everything if good number
      if (res.size == targetSize) res.toVector
      else{
        // tricky part
        // needs last front to be added and remove it ; ! remove the first element of cumsizes
        val lastfrontindex = cumsizes.tail.zipWithIndex.find{case (d,_) =>  d > targetSize}.get._2
        //println("last front index = "+lastfrontindex+" / frontindices size = "+frontindices.size)
        //val lastfront = cumpops.tail(lastfrontindex)
        // indices of individuals in the last front
        val lastfrontinds = frontindices(lastfrontindex)

        val provpop: Vector[I] = cumpops.tail(lastfrontindex - 1)

        // next candidate points to be drawn in lastfront, given ref points
        // -> normalize here
        val (normfitnesses,normreferences) = normalize(allfitnesses,references)

        // niching in association to reference points ; selection according to it
        // needs last front indices
        //implicit val rng = new util.Random
        val additionalPointsIndices = referenceNichingSelection[M](normfitnesses,normreferences,lastfrontinds,targetSize - provpop.size)//(rng=rng)
        val additionalPoints = population.zipWithIndex.filter{case (_,i) => additionalPointsIndices.contains(i)}.map{case (ind,_) => ind}
        provpop++additionalPoints
      }
    }

  }

  /**
    * normalize objectives and compute normalized reference points
    *   - for each dimension :
    *      * compute ideal point
    *      * translate objectives to have min at 0
    *      * compute extreme points
    *   - construct simplex and compute intercepts a_j
    *   - for each dimension, normalize translated objective
    *
    * @param fitnesses
    * @param references
    * @return (normalized fitnesses ; normalized reference points)
    */
  def normalize(fitnesses: Vector[Vector[Double]],references: References): (Vector[Vector[Double]],Vector[Vector[Double]]) = {
    // ideal point, translation and extreme points
    val (translated,maxpoints) = translateAndMaxPoints(fitnesses)
    val intercepts = simplexIntercepts(maxpoints)
    (normalizeMax(translated,intercepts),computeReferencePoints(references,intercepts))
  }

  /**
    * Translate to have ideal point at \vec{0} ; compute max points
    *
    * @param fitnesses
    * @return (translated fitnesses , max points)
    */
  def translateAndMaxPoints(fitnesses: Vector[Vector[Double]]): (Vector[Vector[Double]],Vector[Vector[Double]]) = {
    // TODO check if transpose has expected behavior
    val idealValues = fitnesses.transpose.map{_.min}
    val translated = fitnesses.map{_.zip(idealValues).map{case (f,mi) => f - mi}}
    (translated,translated.transpose.map{v => translated(v.zipWithIndex.maxBy{case (d,_) => d}._2)})
  }

  /**
    * Compute the intercepts on each dimension axis of the simplex generated by the N points given
    * @param maxPoints (MUST have N points to have an hyperplan)
    * @return
    */
  def simplexIntercepts(maxPoints: Vector[Vector[Double]]): Vector[Double] = {
    val firstPoint = maxPoints(0)
    val dim = firstPoint.size
    val translated: Vector[Vector[Double]] = maxPoints.map{_.zip(firstPoint).map{case (xij,x1j) => xij - x1j}}
    val baseChange: RealMatrix = MatrixUtils.createRealMatrix((Vector(firstPoint.map{case xj => - xj})++translated.tail).map{_.toArray}.toArray)
    def getDiag(m: RealMatrix): Vector[Double] = m.getData.zipWithIndex.map{case (row,i) => row(i)}.toVector
    getDiag(MatrixUtils.inverse(baseChange).multiply(MatrixUtils.createRealDiagonalMatrix(Array.fill(dim)(1.0))).add(MatrixUtils.createRealMatrix(Array.fill(dim)(firstPoint.toArray)).transpose()))
  }

  /**
    * normalize to have max at 1
    * @param points
    * @param maxvals
    * @return
    */
  def normalizeMax(points: Vector[Vector[Double]],maxvals: Vector[Double]): Vector[Vector[Double]] =
    points.transpose.zip(maxvals).map{case (p,m) => p.map{_ / m}}.transpose

  /**
    * normalize ref points if needed
    * @param references
    * @param intercepts
    * @return
    */
  def computeReferencePoints(references: References, intercepts: Vector[Double]): Vector[Vector[Double]] = references match {
    case ReferencePoints(references,false) => normalizeMax(references,intercepts)
    case ReferencePoints(references,true) => references
    case r: AutoReferences => References.computeReferences(r,intercepts.length).references //this case shouldnt happen
  }


  /**
    * Aggregate normalized fitnesses on reference points ; select on this.
    * @param normalizedFitnesses
    * @param normalizedReferences
    * @param selectionIndices indices of the set in which to select additional points
    * @param pointsNumber number of points to select
    * @return indices of selected individuals
    *          (population not needed at this stage)
    */
  // FIXME handle random generator through M
  def referenceNichingSelection[M[_]: cats.Monad](
                                 normalizedFitnesses: Vector[Vector[Double]],
                                 normalizedReferences: Vector[Vector[Double]],
                                 selectionIndices: Vector[Int],
                                 pointsNumber: Int
                               ): Vector[Int] = {
    println("Ref points = "+normalizedReferences)
    val assocMap = associateReferencePoints(normalizedFitnesses,normalizedReferences)
    println("association of ref points = "+assocMap)
    var currentRefPoints: Seq[Int] = (0 until normalizedReferences.size) // indices of current ref points
    var currentSelectionIndices = selectionIndices
    //val refCounts = assocMap.toSeq.groupBy{case (_,(j,_)) => j}.toSeq.sortBy(_._1).map{case (_,s)=>s.size}
    val refCount = new mutable.HashMap[Int,Int];for(j <- currentRefPoints){refCount.put(j,0)}
    val toAdd = new ArrayBuffer[Int]
    while (toAdd.length <= pointsNumber) {
      // FIXME add random selection in case of same numbers
      val jmin = refCount.toSeq.filter{case (j,rhoj)=> currentRefPoints.contains(j)}.minBy(_._2)
      // points associated to min niche and in last front
      val candidatePoints = assocMap.toSeq.filter{case (i,(j,d)) => currentSelectionIndices.contains(i)&j==jmin._2}
      if(candidatePoints.size == 0) {
        // remove this reference point
        currentRefPoints = currentRefPoints.filter(_!=jmin._2)
      }else {
        var newpoint = 0
        if(refCount(jmin._2)==0) {
          newpoint = candidatePoints.minBy{case (i,(j,d))=>d}._1
        }else{
          //val randomIndex: Int = randomM.use(rng => rng.nextInt(candidatePoints.size)).asInstanceOf[Int]
          //{} apply randomM.shuffle((0 to candidatePoints.size).toVector).map{_.take(1).head}
          //val randomIndex: M[Int] = Random[M].randomElement((0 to candidatePoints.size).toVector)
          val rng = new util.Random
          val randomIndex = rng.nextInt(candidatePoints.size)
          newpoint = candidatePoints(randomIndex)._1
        }
        toAdd.append(newpoint)
        refCount.put(jmin._2,refCount(jmin._2)+1)
        currentSelectionIndices = currentSelectionIndices.filter(i=> i!= newpoint)
      }
    }
    toAdd.toVector
  }

  /**
    * Compute reference lines, distances, and associate points to references
    * @param points
    * @param references
    * @return map point i => ref point j,distance
    */
  def associateReferencePoints(points: Vector[Vector[Double]],references: Vector[Vector[Double]]): Map[Int,(Int,Double)] = {
    val refnormsquared = references.map{_.map{x => x*x}.sum}
    // FIXME unoptimized, shouldnt recreate the matrices at each run
    def proj(dim: Int,x: Vector[Double]): Vector[Double] = {
      val w = MatrixUtils.createColumnRealMatrix(references(dim).toArray)
      w.multiply(MatrixUtils.createRowRealMatrix(x.toArray)).multiply(w).getColumn(0).map{_ / refnormsquared(dim)}.toVector
    }
    points.zipWithIndex.map {
      case (p,ind) =>
        val dists = (0 until references.length).map {
          i =>
            math.sqrt(p.zip(proj(i,p)).map{case (x,y) => (x - y)*(x - y)}.sum)
        }
        val mindist = dists.min
        (ind,(dists.zipWithIndex.filter{case (d,_)=>d==mindist}.map{case (_,j)=>j}.head,mindist))
    }.toMap
  }







}

