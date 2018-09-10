
package mgobench.problem.coco



case class HistoricalSolution(
                               cocofunction:String,
                               dimension: Int,
                               instance: Int,
                               number_of_objective: Int,
                               bestFitnesses: Vector[Vector[Double]],
                               bestSolutions: Vector[Vector[Double]]
                             )


object HistoricalSolution {

  /**
    * Monoobjective solution
    * @param f
    * @param dim
    * @param instance
    * @param fit
    * @param sol
    */
    def apply(f:String,dim: Int, instance: Int, fit: Double,sol:Vector[Double]): HistoricalSolution = {
      HistoricalSolution(f,dim,instance,1,Vector(Vector(fit)),Vector(sol))
    }

}