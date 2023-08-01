package mgobench.optimise.pso

import mgobench.optimise.Optimisation
import mgobench.problem.Problem
import mgobench.result.Result

/*
import eu.timepit.refined.auto.autoRefineV
import eu.timepit.refined.api.Validate
import eu.timepit.refined.api.RefType
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric
import eu.timepit.refined.numeric.Greater
import scalaz._
import scalaz.effect._
import spire.implicits._
import spire.math.Interval
import cilib._
import cilib.exec._
import cilib.pso._
import cilib.pso.Defaults._
import cilib.benchmarks.Benchmarks._
import mgobench.problem.coco.CocoProblem
import shapeless.Nat
import shapeless.Nat._0
*/

/**
  * Already old benchmark of pso and ga : http://sci-hub.tw/https://ieeexplore.ieee.org/abstract/document/1688379
  * Edwards, A., & Engelbrecht, A. P. (2006, July). Comparing particle swarm optimisation and genetic algorithms for nonlinear mapping. In Evolutionary Computation, 2006. CEC 2006. IEEE Congress on (pp. 694-701). IEEE.
  *
 *
  * @param iterations Number of iterations
  * @param particles Number of particles
  * @param w Weight of inertia
  * @param c1 Weight of cognition
  * @param c2 Weight of social
  */
case class GlobalBestPSO(
                   iterations: Int,
                   particles: Int,
                   w: Double = 0.729844,
                   c1: Double = 1.496180,
                   c2: Double = 1.496180
                        ) extends Optimisation {

  override def optimise(problem: Problem): Result = GlobalBestPSO.optimise(globalBestPSO = this,problem)

  override def name: String = "GBPSO-"+iterations+"-"+particles

}



object GlobalBestPSO {

  /**
    * Optimize with a global best particle swarm
    *
    * @param globalBestPSO
    * @param problem
    * @return
    */
  def optimise(globalBestPSO: GlobalBestPSO, problem: Problem): Result = {

    /*
    val prevevals = problem.evaluations

    val listbounds = problem.lower_bounds.zip(problem.upper_bounds).map{case (l,h)=>Interval(l,h)}.toList
    val bounds: NonEmptyList[Interval[Double]] = NonEmptyList.nel[Interval[Double]](listbounds.head,IList.fromList(listbounds.tail))

    val f: NonEmptyList[Double] => Double = (x: NonEmptyList[Double]) => {
      problem.fitness(IList.fromFoldable(x).toVector)(0)
    }

    val eval: Eval[NonEmptyList, Double] = Eval.unconstrained(f)

    val env = Environment(
      Comparison.dominance(Min),
      eval
    )

    val cognitive = Guide.pbest[Mem[Double], Double]
    val social = Guide.gbest[Mem[Double]]
    val gbestPSO = gbest(globalBestPSO.w, globalBestPSO.c1, globalBestPSO.c2, cognitive, social)

    //val v: Validate[Int,numeric.Positive] = Validate.fromPredicate[Int,numeric.Positive](n => n > 0,n=>n.toString,Greater[_0](Nat._0))
    //val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(bounds, autoRefineV(globalBestPSO.particles)(RefType[Refined],v))
    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(bounds, Refined.unsafeApply(globalBestPSO.particles))
    val iter: Kleisli[Step[Double, ?], NonEmptyList[Particle[Mem[Double], Double]], NonEmptyList[Particle[Mem[Double], Double]]] = Iteration.sync(gbestPSO)

    val problemStream = Runner.staticProblem(name = Refined.unsafeApply(problem.asInstanceOf[CocoProblem].name), env.eval)

    val t = Runner.foldStep(
        env,
        RNG.fromTime,
        swarm,
        Runner.staticAlgorithm[Step[Double,?],NonEmptyList,Particle[Mem[Double], Double]]("gbestPSO", iter),
        problemStream,
        (x: NonEmptyList[Particle[Mem[Double], Double]], _: Eval[NonEmptyList, Double]) =>
          RVar.pure(x)
    )

    val res = t.take(globalBestPSO.iterations).runLast.unsafePerformSync.get.value
    val positions = IList.fromFoldable(res).toList.map{ (p: Particle[Mem[Double], Double])=>p.pos}
    val optValues = positions.map{_.objective.get.fitness.toEither.left.get.asInstanceOf[Feasible].v}
    val optPositions: List[Vector[Double]] = positions.map{_.pos}.map{case l =>IList.fromFoldable(l).toVector}
    val (opt,optindex): (Double,Int) = optValues.zipWithIndex.sortWith{case((d1,_),(d2,_)) => d1 < d2}.head

    Result(Vector(optPositions(optindex)),Vector(Vector(opt)),problem.evaluations - prevevals,problem,globalBestPSO)
    */
    Result.empty
  }



}

