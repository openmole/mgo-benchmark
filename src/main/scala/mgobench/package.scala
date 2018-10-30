import mgobench.optimize._
import mgobench.optimize.pso.{GCPSO, GlobalBestPSO}
import mgobench.optimize.psoakka.BasicPSOAkka
//import mgobench.optimize.ga.NSGA2
import mgobench.problem.coco.{CocoProblem, CocoSolutions, CocoSuite}
import mgobench.result.{Indicators, Result}

import scala.util.Random

package object mgobench {



  def testBenchmark(): Unit = {
    val iterations = 100
    val res: Seq[Result] = Benchmark.benchmark(
      optimizers = Seq(
        mgobench.optimize.RandomSearch(iterations),
        GradientDescent(iterations,tolerance = 1e-5),
        NoisyGradientDescent(iterations,1,1000,1e-5,new Random),
        mgobench.optimize.ga.NSGA2(100,20,1,10000,new Random)
      ),
      nBootstraps = 2,
      suite = CocoSuite.getSuite("bbob"),
      problemsNumber = 1,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )

    utils.io.File.writeCSV(Indicators.computeExpectedIndicators(res),"res/test/test.csv",";")

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
    val iterations = -1

    //Suite.testSuiteOptim("bbob",GradientDescent(iterations))
    val res = Benchmark.benchmark(Seq(GradientDescent(iterations,tolerance = 1e-15)),
      nBootstraps = 1,CocoSuite.getSuite("bbob"),problemsNumber = 5*24*3,
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
