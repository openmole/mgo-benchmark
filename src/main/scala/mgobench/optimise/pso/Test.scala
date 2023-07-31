
package mgobench.optimise.pso



import cilib._
import cilib.pso._
import cilib.pso.Defaults._
import cilib.exec._
import cilib.benchmarks.Benchmarks._
import eu.timepit.refined.auto._
import scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn
import spire.implicits._
import spire.math.Interval


object GBestPSO extends App {

   val bounds = Interval(-5.12, 5.12) ^ 2
    val env = Environment(Comparison.dominance(Min),Eval.unconstrained(spherical[NonEmptyList, Double]))
    //fitness here
    //RVar(RNG.fromTime => (RNG.fromTime,problem.fitness))
    // create pso guides
    val cognitive = Guide.pbest[Mem[Double], Double]
    val social = Guide.gbest[Mem[Double]]
    val gbestPSO = gbest(0.729844, 1.496180, 1.496180, cognitive, social)

    val swarm =
      Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(bounds, 1000)
    val iter = Iteration.sync(gbestPSO)

    val problemStream = Runner.staticProblem("spherical", env.eval)


    val t = Runner.foldStep(
      env,
      RNG.fromTime,
      swarm,
      Runner.staticAlgorithm[Step[Double,?],NonEmptyList,Particle[Mem[Double], Double]]("gbestPSO", iter),
      problemStream,
      (x: NonEmptyList[Particle[Mem[Double], Double]], _: Eval[NonEmptyList, Double]) =>
        RVar.pure(x)
    )

    val finalParticles = t.take(5).runLast.unsafePerformSync.get.value
    //println(finalParticles.head.state)
    //println(finalParticles.head.state.b)
    //println(finalParticles.head.state.v)
    //println(finalParticles.head.pos)

    val optValues = IList.fromFoldable(finalParticles).toList.map{
      (p: Particle[Mem[Double], Double])=>
        p.pos.objective.get.fitness.toEither.left.get.asInstanceOf[Feasible].v}
    println(optValues.min)

}
