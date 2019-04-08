package mgobench.test

import mgo.C
import mgobench.{Benchmark, optimize}
import mgobench.optimize.GradientDescent
import mgobench.optimize.ga.NSGA3Operations.AutoReferences
import mgobench.optimize.ga._
import mgobench.optimize.pso.GCPSO
import mgobench.problem.FitnessSuite
import mgobench.problem.coco.{CocoProblem, CocoSolutions, CocoSuite, NoisyCocoSuite}
import mgobench.problem.noise.GaussianNoise1D
import mgobench.result.{Indicators, Result}


object Test extends App {



  test.testNSGA3()


}




package object test {


  def testBenchmark(): Unit = {
    println("Running test benchmark")
    val iterations = 10000
    val nrepets = 10
    val sigma = 2.0
    val res: Seq[Result] = Benchmark.benchmark(
      optimizers = Seq(
        mgobench.optimize.RandomSearch(iterations / nrepets,nrepets,1),
        mgobench.optimize.GradientDescent(iterations / nrepets, nrepets),
        mgobench.optimize.NoisyGradientDescent(iterations=iterations/(100*nrepets),stochastic_iterations=nrepets,nsearchs=100,tolerance=1e-20),
        mgobench.optimize.ga.NSGA2(lambda = 100, mu = 20,nrepets = 1,generations = (iterations/100) - 1),
        mgobench.optimize.ga.KalmanNSGA2(lambda = 100, mu = 20, generations = (iterations/100)-1, cloneProbability = 0.5,observationNoise = 1.0),
        mgobench.optimize.ga.NoisyNSGA2(lambda=100, mu = 20,generations = (iterations/100)-1,historySize = 100,cloneProbability = 0.2),
        mgobench.optimize.pso.GlobalBestPSO(iterations = iterations / 100,particles = 100)
      ),
      nBootstraps = 1,
      suite = NoisyCocoSuite("bbob",GaussianNoise1D(0,sigma,1)),
      problemsNumber = 1,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )

    println(res.mkString("\n"))
    //utils.io.File.writeCSV(Indicators.computeExpectedIndicators(res),"res/test/test.csv",";")

  }


  def testNSGA3(): Unit = {
    println("Test NSGA3")

    def f(x: Vector[Double]): Vector[Double] = Vector(x(0)+x(1),1/(1+x(0))+x(1))
    val b: Vector[C] = Vector(C(0.0,1.0),C(0.0,1.0))

    val iterations = 10000
    val res = Benchmark.benchmark(Seq(mgobench.optimize.ga.NSGA3(popSize = 100,generations = iterations,referencePoints = AutoReferences(2))),
      nBootstraps = 1,
      suite = FitnessSuite(f,b)
    )

    println(res.mkString("\n"))
  }


  def testNoisyGA(): Unit = {
    println("Running test NoisyNSGA2")
    val iterations = 10000
    val sigma = 2.0
    val res: Seq[Result] = Benchmark.benchmark(
      optimizers = Seq(
        mgobench.optimize.ga.NoisyNSGA2(lambda=100, mu = 20,generations = (iterations/100)-1,historySize = 100,cloneProbability = 0.2)
      ),
      nBootstraps = 1,
      suite = NoisyCocoSuite("bbob",GaussianNoise1D(0,sigma,1)),
      problemsNumber = 1,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )

    println(res.mkString("\n"))
  }

  def testKalmanGA(): Unit = {
    val iterations = 5000
    val res: Seq[Result] = Benchmark.benchmark(
      optimizers = Seq(
        KalmanNSGA2(lambda = 100, mu = 20, generations = iterations-1, cloneProbability = 0.5,observationNoise = 1.0)
      ),
      nBootstraps = 1,
      suite = CocoSuite.getSuite("bbob"),
      problemsNumber = 1,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )
    val hist = CocoSolutions.loadSolutions("data/historicalresults.csv")
    println(res)
    println(Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(0.01,hist),res.toVector))
  }

  def testPSO(): Unit = {
    val iterations = 1000
    val res: Seq[Result] = Benchmark.benchmark(
      optimizers = Seq(
        //BasicPSOAkka(iterations,10) // akka does not work with coco for parallelization
        //GlobalBestPSO(iterations,200)
        GCPSO(iterations,10)
      ),
      nBootstraps = 10,
      suite = CocoSuite.getSuite("bbob"),
      problemsNumber = 2,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )
    val hist = CocoSolutions.loadSolutions("data/historicalresults.csv")
    println(Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(0.01,hist),res.toVector))
  }


  def testGradientDescent(): Unit = {
    val iterations = 100

    //Suite.testSuiteOptim("bbob",GradientDescent(iterations))
    val res = Benchmark.benchmark(Seq(GradientDescent(
      iterations
      ,tolerance = 1e-30
    )),
      nBootstraps = 1,CocoSuite.getSuite("bbob"),problemsNumber = 1,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )
    //println(res)
    val hist = CocoSolutions.loadSolutions("data/historicalresults.csv")
    println(Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(0.001,hist),res.toVector))
  }

  def testRandomSearch(iterations: Int): Unit = {
    //Suite.testSuiteOptim("bbob",GradientDescent(iterations))
    val res = Benchmark.benchmark(Seq(mgobench.optimize.RandomSearch(iterations)),
      nBootstraps = 2,CocoSuite.getSuite("bbob"),problemsNumber = 15*24+2)
    //println(res)
    //val hist = CocoSolutions.loadSolutions("data/historicalresults.csv")
    //println(Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(10.0,hist),res.toVector))
  }



  def testGAOptim(): Unit = {
    // GA optim
    //val results = (0 until 100).map(GA.replication.replication)
    //val deterministic = GA.replication.replication(0)(Rosenbrock.rosenbrock.apply,Rosenbrock.rosenbrock.genome(2))
    //println(deterministic.mkString("\n"))
    //println(deterministic)
    //println(Rosenbrock.counter)
  }

  def resultExtraction(): Unit = {
    //mgobench.problem.coco.CocoSolutions.testResultExtraction()
    mgobench.problem.coco.CocoSolutions.runResultExtraction()
  }

  def testCocoIntegration(): Unit = {
    // test coco integration
    //mgobench.problem.coco.CocoSuite.testSuiteOptim("bbob",optimize.RandomSearch(2))
    mgobench.problem.coco.CocoSuite.testSuiteOptim("bbob-biobj",optimize.RandomSearch(2))
  }

}


