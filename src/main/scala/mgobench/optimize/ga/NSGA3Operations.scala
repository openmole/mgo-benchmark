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
import org.apache.commons.math3.util._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

import mgobench.utils.implicits._

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
    * Body of fractions
    *  - used for tricking comparison of vectors and have exact discrete points
    * @param n
    * @param d
    * @param reduced
    */
  case class Fraction(n:Int,d:Int,reduced: Boolean){
    def +(f: Fraction): Fraction = Fraction((n*f.d + d*f.n ),d*f.d)
    def -(f: Fraction): Fraction = Fraction(n*f.d - d*f.n,d*f.d)
    def *(f: Fraction): Fraction = Fraction(n*f.n,d*f.d)
    def /(f: Fraction): Fraction = Fraction(n*f.d,d*f.n)
    def *(p: Point): Point = Point(p.point.map{_*this})
    def toDouble: Double = n.toDouble/d.toDouble
    def isPositive: Boolean = (n>=0&&d>=0)||(n<=0&&d<=0)
  }
  object Fraction {
    val zero = Fraction(0,1)
    val one = Fraction(1,1)
    def apply(x: Int): Fraction = Fraction(x,1)
    def apply(n: Int,d: Int): Fraction = {
      // sign always at numerator the way fractions are constructed ?
      val gcd = ArithmeticUtils.gcd(n, d)
      Fraction((n / gcd).toInt, (d / gcd).toInt,true)
    }
  }

  /**
    * Fractional points
    * @param point
    */
  case class Point(point: Vector[Fraction]){
    def +(p: Point): Point = Point(point.zip(p.point).map{case(f1,f2)=>f1+f2})
    def -(p: Point): Point = Point(point.zip(p.point).map{case(f1,f2)=>f1-f2})
    def toDoubleVector: Vector[Double] = point.map(_.toDouble)
    def embedded(i: Int): Point = Point((0 to i-1 by 1).map{i => point(i)}.toVector ++ Vector(Fraction.zero) ++  (i until point.size by 1).map{i => point(i)}.toVector)
    //def isPositive: Boolean = point.map{case Fraction(n,_,_) => n>0}.reduce(_&_)
    def isPositive: Boolean = point.map{_.isPositive}.reduce(_&&_)
    def isOnSimplex: Boolean = (point.reduce(_+_) == Fraction.one)&&isPositive // not the full simplex
  }

  case class DiscreteUnitSimplex(
                                dimension: Int,
                                divisions: Int,
                          /**
                            * discrete point in the simplex
                            */
                          points: Vector[Point]//,

                                /**
                                  * n-1 generator vectors of dim n
                                  */
                          //generators: Vector[Point]
                        ) {
    def embeddedPoints(i : Int): Vector[Point] = points.map(_.embedded(i))
  }

  object DiscreteUnitSimplex {

    /**
      * basic case : two dimensional simplex
      * @param divisions
      * @return
      */
    def twoDimSimplex(divisions: Int): DiscreteUnitSimplex = {
      val coords = (0 to divisions by 1).map{Fraction(_,divisions)}
      val points = coords.zip(coords.reverse).map{case (f1,f2) => Point(Vector(f1,f2))}.toVector
      //val generator = points(1) - points(0)
      //DiscreteUnitSimplex(2,divisions,points,Vector(generator))
      DiscreteUnitSimplex(2,divisions,points)
    }

   // def embeddedGenerators(simplex: DiscreteUnitSimplex,direction: Int): Vector[Point] = simplex.generators.map(_.embedded(direction))

    /**
      * recursive constructor
      * two complementary hypersimplices are anough to generate the simplex in the next dimension
      * (rough algorithm by filtering points - still a polynomial upper bound)
      *
      * @param dimension
      * @param divisions
      * @return
      */
    def apply(dimension: Int, divisions: Int): DiscreteUnitSimplex = {
      dimension match {
        case 2 => twoDimSimplex(divisions)
        case _ => {
          val prevSimplex = DiscreteUnitSimplex(dimension - 1,divisions)
          val emb0 = prevSimplex.embeddedPoints(0)
          val emb1 = prevSimplex.embeddedPoints(1)
          val origin = emb0(0)
          val points = (for {
            vi <- emb0.map(_ - origin)
            vj <- emb1.map(_ - origin)
          } yield origin + vi + vj).filter(_.isOnSimplex).distinct

          DiscreteUnitSimplex(dimension,divisions,points)
        }
      }
    }

  }

  /**
    * compute auto ref points on the simplex
    *   (called at initialization)
    * @param divisions number of segments on each simplex bord line
    * @param dimension dimension of the space
    * @return
    */
  def simplexRefPoints(divisions: Int,dimension: Int): Vector[Vector[Double]] = {
    //println("Computing simplex reference points with "+divisions+" divisions in dimension "+dimension)
    //val start = System.currentTimeMillis()
    val res = DiscreteUnitSimplex(dimension,divisions).points.map{_.toDoubleVector}
    //println("n = "+dimension+" ; p = "+divisions+" ; t = "+(System.currentTimeMillis() - start))
    res
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
          mu: Int
                                                      ): Elitism[M,I] = Elitism[M, I] { population =>
    println("elitism - pop size "+population.size)
    for {
      // still remove clone for deterministic nsga3
      cloneRemoved <- applyCloneStrategy(values, keepFirst[M, I]) apply filterNaN(population, fitness)
      //fronts = successiveFronts(cloneRemoved,fitness) // computed in eliteWithReference
      elite = eliteWithReference[M,I](cloneRemoved,fitness, references,mu)
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

    // DEBUG ascii plot
    //import org.sameersingh.scalaplot.Implicits._
    //output(ASCII,org.sameersingh.scalaplot.Implicits.xyChart((fitnesses.map{_(0)},fitnesses.map{_(1)})))

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
                            mu: Int
                           ): Vector[I] = {
    println("elite with ref - pop size "+population.size)
    val allfronts = successiveFronts(population,fitness)
    println("number of pareto fronts = "+allfronts.size)
    val fronts = allfronts.map{_._1}
    //println("front sizes = "+fronts.map{_.size})
    val fitnesses = allfronts.map{_._2}
    val frontindices = allfronts.map{_._3}
    val allfitnesses = fitnesses.reduce{_++_}//{(v:(Vector[Vector[Double]],Vector[Vector[Double]])) => v._1++v._2} // dirty flatten

    //val targetSize = mu(population)
    val targetSize = mu

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

      println("cumulated front sizes : "+cumsizes)

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

        val provpop: Vector[I] = if (lastfrontindex > 0) cumpops.tail(lastfrontindex - 1) else Vector.empty
        println("previous pop size = "+provpop.size)

        // next candidate points to be drawn in lastfront, given ref points
        // -> normalize here
        val (normfitnesses,normreferences) = normalize(allfitnesses,references)

        // niching in association to reference points ; selection according to it
        // needs last front indices
        //implicit val rng = new util.Random
        //val additionalPointsIndices = referenceNichingSelection[M](normfitnesses,normreferences,lastfrontinds,targetSize - provpop.size)//(rng=rng)
        //val additionalPoints = population.zipWithIndex.filter{case (_,i) => additionalPointsIndices.contains(i)}.map{case (ind,_) => ind}
        val additionalPoints = referenceNichingSelection[M,I](filter[Vector[Double]](normfitnesses,lastfrontinds),filter[Vector[Double]](normreferences,lastfrontinds),filter[I](population,lastfrontinds),targetSize - provpop.size)
        println("size of final elite population : "+provpop.size+" + "+additionalPoints.size)
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
    * @return (translated fitnesses , max points indices for each dimension)
    */
  def translateAndMaxPoints(fitnesses: Vector[Vector[Double]]): (Vector[Vector[Double]],Vector[Vector[Double]]) = {
    // TODO check if transpose has expected behavior
    val idealValues = fitnesses.transpose.map{_.min}
    println("mins = "+idealValues)
    println("maxs = "+fitnesses.transpose.map{_.max})
    val translated = fitnesses.map{_.zip(idealValues).map{case (f,mi) => f - mi}}
    assert(translated.flatten.min >= 0.0,"negative translated data")
    (translated,translated.transpose.map{v => translated(v.zipWithIndex.minBy{case (d,_) => d}._2)})
  }

  /**
    * Compute the intercepts on each dimension axis of the simplex generated by the N points given
    * @param maxPoints (MUST have N points to have an hyperplan)
    * @return
    */
  def simplexIntercepts(maxPoints: Vector[Vector[Double]]): Vector[Double] = {
    val firstPoint = maxPoints(0)
    val dim = firstPoint.size

    // FIXME problem with translation here ?
    val translated: Vector[Vector[Double]] = maxPoints.map{_.zip(firstPoint).map{case (xij,x1j) => xij - x1j}}
    val baseChange: RealMatrix = MatrixUtils.createRealMatrix((Vector(firstPoint.map{case xj => - xj})++translated.tail).map{_.toArray}.toArray)

    // check that the new basis is not singular
    assert((new LUDecomposition(baseChange)).getDeterminant!=0,"singular matrix : "+baseChange.toString+"\n max points are : "+maxPoints)

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
  def referenceNichingSelection[M[_]: cats.Monad: Random,I](
                                 normalizedFitnesses: Vector[Vector[Double]],
                                 normalizedReferences: Vector[Vector[Double]],
                                 //selectionIndices: Vector[Int],
                                 population: Vector[I],
                                 pointsNumber: Int
                               ): Vector[I] = {
    //println("Ref points = "+normalizedReferences)
    println("Adding "+pointsNumber+" points among "+population.size)
    println(normalizedFitnesses)
    // FIXME normalized fitnesses are wrong

    val normFitnessMap = (population.zip(normalizedFitnesses)).toMap
    val assocMap = associateReferencePoints(normalizedFitnesses,normalizedReferences,population) // associate points to references
    //println("association of ref points = "+assocMap)
    val (finalAssocMap,selected) = pointsSelection(assocMap,Vector.empty,pointsNumber)(new util.Random)
    println("distinct niched ref points = "+selected.map{_._2}.distinct)
    println("rel min x sel points = "+(selected.map(s => normFitnessMap(s._1)(0)).min))
    println("rel min y sel points = "+(selected.map(s => normFitnessMap(s._1)(1)).min))
    selected.map{_._1}
  }

  /**
    * Compute reference lines, distances, and associate points to references
    * @param points
    * @param references
    * @param selectionIndices filter final point on these indices
    * @return map point i => ref point j,distance
    */
  def associateReferencePoints[I](points: Vector[Vector[Double]],references: Vector[Vector[Double]],population: Vector[I]): Map[I,(Int,Double)] = {
    val refnormsquared = references.map{_.map{x => x*x}.sum}
    // FIXME unoptimized, shouldnt recreate the matrices at each run
    def proj(dim: Int,x: Vector[Double]): Vector[Double] = {
      val w = MatrixUtils.createColumnRealMatrix(references(dim).toArray)
      w.multiply(MatrixUtils.createRowRealMatrix(x.toArray)).multiply(w).getColumn(0).map{_ / refnormsquared(dim)}.toVector
    }
    points.zip(population).map {
      case (p,ind) =>
        val dists = (0 until references.length).map {
          i =>
            math.sqrt(p.zip(proj(i,p)).map{case (x,y) => (x - y)*(x - y)}.sum)
        }
        val mindist = dists.min
        (ind,(dists.zipWithIndex.filter{case (d,_)=>d==mindist}.map{case (_,j)=>j}.head,mindist))
    }.toMap
  }

  def pointsSelection[I](associationMap: Map[I,(Int,Double)],selected: Vector[(I,Int)],toselect: Int)(implicit rng: util.Random): (Map[I,(Int,Double)],Vector[(I,Int)]) = {
    //println("Selecting "+toselect+" points from "+associationMap.toVector.size)
    toselect match {
      case n if n == 0 => (associationMap,selected)
      case _ => {
        //val refCount = selected.groupBy(_._2).map{g => (g._1,g._2.size)}
        //val refCount = associationMap.toVector.groupBy(_._2._1).map{g => (g._1,g._2.size)} // ref with no count can not be in the refcount
        val selectedRefCount = selected.groupBy(_._2).map{g => (g._1,g._2.size)}
        val refCount = associationMap.map{_._2._1}.toVector.distinct.map{j => (j,selectedRefCount.getOrElse(j,0))}.toMap
        val (jmin,_) = refCount.toVector.minBy(_._2) // index of ref point with minimal number of associated points
        val candidatePoints = associationMap.filter{case (_,(j,_)) => j==jmin} // cannot be 0 the way it is constructed
        //val newpointIndex = if(refCount(jmin)==0) candidatePoints.minBy{_._2._2}._1 else  candidatePoints.minBy{_._2._2}._1
        //val newpointIndex = candidatePoints.minBy{_._2._2}._1 // taking the min dist point leads to an overcrowding of some points only
        val newpoint = if(refCount(jmin)==0) candidatePoints.toVector.minBy{_._2._2}._1 else {
          candidatePoints.toVector(rng.nextInt(candidatePoints.toVector.size))._1
        }
          pointsSelection(associationMap.filter{_._1!=newpoint},selected++Vector((newpoint,jmin)),toselect - 1)
      }
    }
  }







}

