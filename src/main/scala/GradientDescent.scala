
//import dynaml.optimization.GradientDescent
// impossible to use dynaml

import breeze.linalg.DenseVector
import breeze.optimize.ApproximateGradientFunction
import breeze.optimize.LBFGS


case class GradientDescent (
                           iterations: Int//,
                           //x0: Vector[Double] // strange to dissociate x0 from the problem
                           ) extends Optimization {

  override def optimize(problem: Problem): Result = GradientDescent.optimize(this,problem)

}


object GradientDescent {


  /*
  def apply(iterations: Int, problem: Problem) = GradientDescent(iterations,
    problem.boundaries.map{case c => (c.low + c.high) / 2}
  )*/



  def optimize(gradientDescent: GradientDescent,problem: Problem): Result = {

    // gradient descent is a monoobjective optimization : we aggregate by summing here
    val fitness: DenseVector[Double] => Double = {x => problem.fitness(x.toScalaVector()) sum}
    val boundaries = problem.boundaries

    // define gradient function
    val gradient = new ApproximateGradientFunction(fitness)

    // define the solver
    println("Running LBFGS on problem "+problem.toString)
    val lbfgs = new LBFGS[DenseVector[Double]](maxIter=gradientDescent.iterations, m=3)

    //val res = lbfgs.minimize(gradient,new DenseVector(gradientDescent.x0.toArray))
    val minstate = lbfgs.minimizeAndReturnState(gradient,new DenseVector(problem.boundaries.map{case c => (c.low + c.high)/2}.toArray))

    // FIXME huge issue with stochastic functions for now
    Result(Vector(minstate.x.toScalaVector()),Vector(problem.fitness(minstate.x.toScalaVector())),Vector(Vector.fill(problem.number_of_objectives)(0.0)),minstate.iter)
  }

}





