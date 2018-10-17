
package mgobench.optimize

import mgobench.problem.Problem
import mgobench.result.Result

import scala.util.Random

case class NoisyGradientDescent(
                               iterations: Int,
                               stochastic_iterations: Int,
                               nsearchs: Int,
                               tolerance: Double = 1e-5,
                               rng: Random = new Random
                               ) extends Optimization {

  override def optimize(problem: Problem): Result = NoisyGradientDescent.optimize(this,problem)

  override def name: String = "NGD-"+iterations+"-"+stochastic_iterations+"-"+nsearchs+"-"+tolerance
}



object NoisyGradientDescent {

  def optimize(noisyGradientDescent: NoisyGradientDescent,problem: Problem): Result = {
    val solver = GradientDescent(noisyGradientDescent.iterations,stochastic_iterations=noisyGradientDescent.stochastic_iterations,tolerance=noisyGradientDescent.tolerance,
      x0 = { problem =>
        problem.boundaries.map{case c => noisyGradientDescent.rng.nextDouble()*(c.high - c.low) + c.low}.toArray
      }
    )
    val results = (1 to noisyGradientDescent.nsearchs).map{_=>GradientDescent.optimize(solver,problem)}
    //results.foreach{case r => println(r.values(0)(0))}
    val bestres = results.sortWith{case (r1,r2) => r1.values(0)(0) < r2.values(0)(0) }(0)
    Result(bestres.points,bestres.values,results.map(_.runs).sum,problem,noisyGradientDescent)
  }

}

