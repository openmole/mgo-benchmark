
package mgobench



import mgobench.optimize.{NoisyGradientDescent, _}
import mgobench.optimize.ga._
import mgobench.optimize.pso._
import mgobench.problem.coco.{CocoProblem, NoisyCocoSuite}
import mgobench.problem.noise.GaussianNoise1D
import mgobench.result.{Indicators, Result}
import java.text.SimpleDateFormat
import java.util.Date


/**
  * sbt run optimName nBootstraps seed iterations nrepets sigma lambda particles
  */
object Launcher extends App {

  assert(args.length==8,s"Wrong arg number ${args.length} ; ${args.mkString(" ")}")

  val optimName = args(0)
  val nBootstraps = args(1).toInt

  /**
    * Seed of the noise
    */
  val seed: Int = args(2).toInt

  /**
    * number of iterations should approximately be the number of function calls
    */
  val iterations: Int = args(3).toInt

  /**
    * number of local repets (use depends on the method)
    */
  val nrepets: Int = args(4).toInt

  /**
    * precision to evaluate quality of results
    *  -> not needed here, can be introduced only in postprocessing
    */
  //val epsilon: Double = args(5).toDouble

  val sigma: Double = args(5).toDouble

  // optimizer specific params
  val lambda: Int = args(6).toInt
  val particles: Int = args(7).toInt

  val rs = RandomSearch(iterations / nrepets,nrepets,seed)
  val gd = GradientDescent(iterations)
  val ngd = NoisyGradientDescent(1000,nrepets,iterations/1000)
  val nsga2 = NSGA2(lambda,1,nrepets,iterations / nrepets)
  val noisynsga2 = NoisyNSGA2(lambda,1,iterations,historySize = 100,cloneProbability = 0.2)
  val pso = GlobalBestPSO(iterations / particles,particles)

  val optimizers: Seq[Optimization] = optimName match {
    case "RS" => Seq(rs)
    case "GD" => Seq(gd)
    case "NGD" => Seq(ngd)
    case "NSGA2" => Seq(nsga2)
    case "NNSGA2" => Seq(noisynsga2)
    case "GBPSO" => Seq(pso)
    case "all" => Seq(rs,gd,ngd,nsga2,noisynsga2,pso)
  }

  val res: Seq[Result] = Benchmark.benchmark(
    optimizers = optimizers,
    nBootstraps = nBootstraps,
    suite = NoisyCocoSuite("bbob",GaussianNoise1D(0,sigma,seed)),
    problemsNumber = 1000,
    problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
  )

  val ts = ((new SimpleDateFormat("yyyyMMdd_HHmmss")).format(new Date()))

  // write results
  utils.io.File.writeCSV(Indicators.computeSuccesses(res),"res/"+ts+"_"+optimName+".csv",";")

  //write parameters
  utils.io.File.writeCSV(
    Array(
      Array("optimName","nBootstraps","seed","iterations","nrepets","sigma","lambda","particles").asInstanceOf[Array[Any]],
      Array(optimName,nBootstraps,seed,iterations,nrepets,sigma,lambda,particles).asInstanceOf[Array[Any]]
    )
  ,file = "res/"+ts+"_"+optimName+"_params.csv",delimiter = ";")
}
