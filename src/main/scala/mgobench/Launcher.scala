
package mgobench


import mgobench.optimise.*
import mgobench.optimise.ga._
import mgobench.optimise.pso._
import mgobench.problem.coco.{CocoProblem, NoisyCocoSuite}
import mgobench.problem.noise.{GaussianNoise1D, Noise}
import mgobench.result.{Indicators, Result}
import java.text.SimpleDateFormat
import java.util.Date


/**
  * sbt run optimName noiseName nBootstraps seed iterations nrepets sigma gradientnsearchs lambda mu particles kalmanCloneProba kalmanObservationNoise noisyNsga2historySize noisyNsga2cloneProba
  */
object Launcher extends App {

  assert(args.length==15,s"Wrong arg number ${args.length} ; ${args.mkString(" ")}")

  val optimName = args(0)
  val noiseName = args(1)
  val nBootstraps = args(2).toInt

  /**
    * Seed of the noise
    */
  val seed: Int = args(3).toInt
    //new util.Random().nextInt()//

  /**
    * approximate number of function calls = budget
    *
    * Fixed param in NoisyGradient => budget / nrepets >= 100
    *
    */
  val budget: Int = args(4).toInt


  /**
    * number of local repets (use depends on the method)
    */
  val nrepets: Int = args(5).toInt

  /**
    * precision to evaluate quality of results
    *  -> not needed here, can be introduced only in postprocessing
    */
  //val epsilon: Double = args(5).toDouble

  val sigma: Double = args(6).toDouble

  // optimizer specific params
  val gradientnsearchs = args(7).toInt
  val lambda: Int = args(8).toInt
  val mu: Int = args(9).toInt // default 20
  val particles: Int = args(10).toInt
  val kalmanCloneProba: Double = args(11).toDouble // default 0.5
  val kalmanObservationNoise: Double = args(12).toDouble // default 1.0 (?)
  val noisyNsga2historySize: Int = args(13).toInt // default to 100
  val noisyNsga2cloneProba: Double = args(14).toDouble //default to 0.2

  val rs = RandomSearch(budget / nrepets,nrepets,seed)
  val gd = GradientDescent(budget / nrepets,nrepets)
  val ngd = NoisyGradientDescent(iterations=budget/(gradientnsearchs*nrepets),stochastic_iterations=nrepets,nsearchs=gradientnsearchs,tolerance=1e-20)
  val nsga2 = NSGA2(lambda = lambda,mu = mu,nrepets = nrepets,generations = (budget / (nrepets * lambda)) - 1)
  val kalmannsga2 = KalmanNSGA2(lambda = lambda, mu = mu, generations = (budget/lambda)-1, cloneProbability = kalmanCloneProba,observationNoise = kalmanObservationNoise)
  val noisynsga2 = NoisyNSGA2(lambda = lambda,mu = mu,generations = (budget / lambda),historySize = noisyNsga2historySize,cloneProbability = noisyNsga2cloneProba,embedding="inv-size")
  val pso = GlobalBestPSO(iterations = budget / particles,particles = particles)

  val optimizers: Seq[Optimisation] = optimName match {
    case "RS" => Seq(rs)
    case "GD" => Seq(gd)
    case "NGD" => Seq(ngd)
    case "NSGA2" => Seq(nsga2)
    case "NNSGA2" => Seq(noisynsga2)
    case "GBPSO" => Seq(pso)
    case "all" => Seq(rs,gd,ngd,nsga2,kalmannsga2,noisynsga2,pso)
  }

  val noise: Noise = noiseName match {
      // only gaussian for now
    case "Gaussian" => GaussianNoise1D(0,sigma,seed)
    case  _ => GaussianNoise1D(0,sigma,seed)
  }

  val res: Seq[Result] = Benchmark.benchmark(
    optimizers = optimizers,
    nBootstraps = nBootstraps,
    suite = NoisyCocoSuite("bbob",noise),
    problemsNumber = 1000,
    problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
  )

  val ts = ((new SimpleDateFormat("yyyyMMdd_HHmmss")).format(new Date()))

  // write results
  utils.io.File.writeCSV(Indicators.computeSuccesses(res),"res/"+ts+"_"+optimName+".csv",";")

  //write parameters
  utils.io.File.writeCSV(
    Array(
      Array("optimName","noiseName","nBootstraps","seed","budget","nrepets","sigma","gradientnsearchs","lambda","mu","particles","kalmanCloneProba","kalmanObservationNoise","noisyNsga2historySize","noisyNsga2cloneProba").asInstanceOf[Array[Any]],
      Array(optimName,noiseName,nBootstraps,seed,budget,nrepets,sigma,gradientnsearchs,lambda,mu,particles,kalmanCloneProba,kalmanObservationNoise,noisyNsga2historySize,noisyNsga2cloneProba).asInstanceOf[Array[Any]]
    )
  ,file = "res/"+ts+"_"+optimName+"_params.csv",delimiter = ";")
}
