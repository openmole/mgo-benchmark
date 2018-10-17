
package mgobench

import mgobench.optimize.ga.NSGA2
import mgobench.optimize.{GradientDescent, NoisyGradientDescent, Optimization, RandomSearch}


object Launcher extends App {

  val optimName = args(0)

  // Params : should be passed as command line
  val seed: Int = 0
  val iterations: Int = 100
  val nrepets: Int = 1

  val optimizers: Seq[Optimization] = optimName match {
    case "RS" => Seq(RandomSearch(iterations,nrepets,seed))
    case "GD" => Seq(GradientDescent(iterations))
    case "NGD" => Seq(NoisyGradientDescent(iterations,nrepets,100))
    case "NSGA2" => Seq(NSGA2(100,10,nrepets,10000))
    case "NNSGA2" => Seq.empty
    case "all" => Seq.empty
  }


}
