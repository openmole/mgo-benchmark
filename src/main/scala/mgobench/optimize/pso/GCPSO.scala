
package mgobench.optimize.pso

import cilib.exec.Runner
import cilib.pso.Defaults
import cilib.pso.{Guide, PSO, Particle}
import cilib.{Comparison, Entity, Environment, Eval, Feasible, Iteration, Mem, Min, Position, RNG, Step, StepS}
import eu.timepit.refined.api.Refined
import mgobench.optimize.Optimization
import mgobench.problem.Problem
import mgobench.result.Result
import scalaz.effect.IO
import scalaz.effect.IO.putStrLn
import scalaz.{IList, Kleisli, NonEmptyList}
import spire.math.Interval
import spire.algebra.{Order, Rng}
//import spire.std.any.DoubleAlgebra.

import scala.util.Random


case class GCPSO(
                  /**
                    * Number of iterations
                    */
                iterations: Int,

                  /**
                    * Number of particles
                    */

                  particles: Int,

                  w: Double = 0.729844,

                  c1: Double = 1.496180,

                  c2: Double = 1.496180,

                  rng: Random = new Random

                ) extends Optimization {

  override def optimize(problem: Problem): Result = GCPSO.optimize(gcpso = this,problem)

  override def name: String = "GCPSO-"+iterations+"-"+particles+"-"+w+"-"+c1+"_"+c2

}




object GCPSO {

  def optimize(gcpso: GCPSO,problem: Problem): Result = {

    val prevevals = problem.evaluations

    val listbounds = problem.lower_bounds.zip(problem.upper_bounds).map{case (l,h)=>Interval(l,h)(Order.from{case d=> (d._1-d._2).ceil.toInt})}.toList
    val bounds: NonEmptyList[Interval[Double]] = NonEmptyList.nel[Interval[Double]](listbounds.head,IList.fromList(listbounds.tail))

    val f: NonEmptyList[Double] => Double = (x: NonEmptyList[Double]) => {
      problem.fitness(IList.fromFoldable(x).toVector)(0)
    }

    val eval: Eval[NonEmptyList, Double] = Eval.unconstrained(f)

    val env = Environment(
      Comparison.dominance(Min),
      eval
    )

    // Define a normal GBest PSO and run it for a single iteration
    val cognitive = Guide.pbest[Mem[Double], Double]
    val social = Guide.gbest[Mem[Double]]
    val gcPSO: NonEmptyList[Particle[Mem[Double], Double]] => Particle[Mem[Double], Double] => StepS[
      Double,
      PSO.GCParams,
      Particle[Mem[Double], Double]] =
      Defaults.gcpso(w = gcpso.w, c1 = gcpso.c1 , c2 = gcpso.c2, cognitive)

    val iter: Kleisli[StepS[Double, PSO.GCParams, ?],
      NonEmptyList[Particle[Mem[Double], Double]],
      NonEmptyList[Particle[Mem[Double], Double]]] =
      Iteration.syncS(gcPSO)

    final object LocalRng extends Rng[Double] {
      override def zero: Double = 0.0
      override def times(x: Double,y: Double): Double = x*y
      override def plus(x: Double,y: Double): Double = x+y
      override def negate(x: Double): Double = -1.0*x
    }

    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed(LocalRng)), x)))(bounds, Refined.unsafeApply(gcpso.iterations))


    val algParams = PSO.defaultGCParams

    // RNG.fromTime provides a time-seeded rng
    val res = Runner.repeat[StepS[Double, PSO.GCParams, ?],NonEmptyList,Particle[Mem[Double], Double]](gcpso.iterations, iter, swarm).run(algParams).run(env).run(RNG.init(gcpso.rng.nextLong()))._2.toEither.right.get._2

    val positions = IList.fromFoldable(res).toList.map{ (p: Particle[Mem[Double], Double])=>p.pos}
    val optValues = positions.map{_.objective.get.fitness.toEither.left.get.asInstanceOf[Feasible].v}
    val optPositions: List[Vector[Double]] = positions.map{_.pos}.map{case l =>IList.fromFoldable(l).toVector}
    val (opt,optindex): (Double,Int) = optValues.zipWithIndex.sortWith{case((d1,_),(d2,_)) => d1 < d2}.head

    Result(Vector(optPositions(optindex)),Vector(Vector(opt)),problem.evaluations - prevevals,problem,gcpso)
  }

}