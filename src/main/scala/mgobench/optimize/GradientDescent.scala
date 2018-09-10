package mgobench.optimize


//import dynaml.optimization.mgobench.optimize.GradientDescent
// impossible to use dynaml

import breeze.linalg.DenseVector
import breeze.optimize.{ApproximateGradientFunction, DiffFunction, LBFGS}
import mgobench.problem.Problem
import mgobench.result.Result


case class GradientDescent (
                             /**
                               * number of iterations
                               */
                           iterations: Int,

                             /**
                               * number of repetitions to evaluate a stochastic gradient
                               */
                           stochastic_iterations: Int = 1,

                             /**
                               * step to compute gradient
                               */
                             epsilon: Double = 1e-5
                           ) extends Optimization {

  override def optimize(problem: Problem): Result = GradientDescent.optimize(this,problem)

}


object GradientDescent {

  /**
    * Default run
    */
  val default: GradientDescent = GradientDescent(10000)

  class StochasticApproximateGradient(f: DenseVector[Double]=>Double, n : Int, epsilon: Double) extends DiffFunction[DenseVector[Double]] {
    override def valueAt(x: DenseVector[Double]) = f(x)

    val gradient = new ApproximateGradientFunction(f)

    def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
      val grads = (1 to n by 1).map{case i => gradient.calculate(x)}
      (grads.map(_._1).sum / n,grads(0)._2)
    }
  }


  /**
    * Optimize a given problem with a given solver
    *
    * @param gradientDescent
    * @param problem
    * @return
    */
  def optimize(gradientDescent: GradientDescent,problem: Problem): Result = {

    // gradient descent is a monoobjective optimization : we aggregate by summing here
    val fitness: DenseVector[Double] => Double = {x => problem.fitness(x.toScalaVector()) sum}
    val boundaries = problem.boundaries

    // define gradient function
    //val gradient = new ApproximateGradientFunction(fitness)
    val gradient = new StochasticApproximateGradient(fitness,gradientDescent.stochastic_iterations,gradientDescent.epsilon)

    // define the solver
    println("Running LBFGS on problem "+problem.toString)
    val lbfgs = new LBFGS[DenseVector[Double]](maxIter=gradientDescent.iterations, m=3)

    //val res = lbfgs.minimize(gradient,new DenseVector(gradientDescent.x0.toArray))
    val minstate = lbfgs.minimizeAndReturnState(gradient,new DenseVector(problem.boundaries.map{case c => (c.low + c.high)/2}.toArray))

    Result(
      points = Vector(minstate.x.toScalaVector()),
      values =Vector(problem.fitness(minstate.x.toScalaVector())),
      precisions = Vector(Vector.fill(problem.number_of_objectives)(0.0)),
      runs = minstate.iter
    )
  }

}





