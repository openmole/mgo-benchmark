
package mgobench.optimize

import mgobench.problem.Problem
import mgobench.result.Result

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class NoisyGradientDescent(
                               iterations: Int,
                               stochastic_iterations: Int,
                               nsearchs: Int,
                               budget: Int,
                               tolerance: Double = -1,
                               rng: Random = new Random
                               ) extends Optimization {

  override def optimize(problem: Problem): Result = NoisyGradientDescent.optimize(this,problem)

  override def name: String = "NGD-"+iterations+"-"+stochastic_iterations+"-"+nsearchs+"-"+tolerance
}



object NoisyGradientDescent {

  def apply(iterations: Int, stochastic_iterations: Int, nsearchs: Int,tolerance: Double): NoisyGradientDescent =
    NoisyGradientDescent(iterations,stochastic_iterations,nsearchs,iterations*stochastic_iterations*nsearchs)

  def optimize(noisyGradientDescent: NoisyGradientDescent,problem: Problem): Result = {
    val solver = GradientDescent(noisyGradientDescent.iterations,stochastic_iterations=noisyGradientDescent.stochastic_iterations,tolerance=noisyGradientDescent.tolerance,
      x0 = { problem =>
        problem.boundaries.map{case c => noisyGradientDescent.rng.nextDouble()*(c.high - c.low) + c.low}.toArray
      }
    )
    //val results =(1 to math.max(1,noisyGradientDescent.nsearchs)).map{_=>GradientDescent.optimize(solver,problem)}
    //results.foreach{case r => println(r.values(0)(0))}
    val results = new ArrayBuffer[Result]()
    var iters = 0
    while (iters < noisyGradientDescent.budget){
      results.append(GradientDescent.optimize(solver,problem))
      iters=iters+results.takeRight(1)(0).runs
    }

    val bestres = results.sortWith{case (r1,r2) => r1.values(0)(0) < r2.values(0)(0) }(0)
    Result(bestres.points,bestres.values,results.map(_.runs).sum,problem,noisyGradientDescent)
  }

}

