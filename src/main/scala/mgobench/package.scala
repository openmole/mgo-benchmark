import mgobench.optimize._
//import mgobench.optimize.ga.NSGA2
import mgobench.problem.coco.{CocoProblem, CocoSolutions, CocoSuite}
import mgobench.result.Indicators

import scala.util.Random

package object mgobench {


  def testBenchmark(): Unit = {
    val iterations = 100
    val res = Benchmark.benchmark(Seq(
      GradientDescent(iterations,tolerance = 1e-5),
      //mgobench.optimize.RandomSearch(iterations),
      NoisyGradientDescent(iterations,1,1000,1e-5,new Random),
      mgobench.optimize.ga.NSGA2(100,20,1,10000,new Random)
    ),
      nBootstraps = 10,CocoSuite.getSuite("bbob"),problemsNumber = 5,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )
    val hist = CocoSolutions.loadSolutions("data/historicalresults.csv")
    println(res.groupBy(_.id).map {
      case (k,results) =>
      //println("Expected runtimes for "+k+" : " + Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(0.1, hist), results.toVector))
        k+" : " + Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(0.1, hist), results.toVector)
    }.toList.sorted.mkString("\n"))
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
