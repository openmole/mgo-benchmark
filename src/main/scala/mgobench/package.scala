import mgobench.optimize._
import mgobench.problem.coco.{CocoSolutions, CocoSuite}
import mgobench.result.Indicators


package object mgobench {


  def resultExtraction(): Unit = {
    //mgobench.problem.coco.CocoSolutions.testResultExtraction()
    mgobench.problem.coco.CocoSolutions.runResultExtraction()
  }



  def testGradientDescent(): Unit = {
    val iterations = -1

    //Suite.testSuiteOptim("bbob",GradientDescent(iterations))
    val res = Benchmark.benchmark(Seq(GradientDescent(iterations,tolerance = 1e-15)),
      nBootstraps = 1,CocoSuite.getSuite("bbob"),problemsNumber = 5*24*3)
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

  def testCocoIntegration(): Unit = {
    // test coco integration
    //mgobench.problem.coco.CocoSuite.testSuiteOptim("bbob",optimize.RandomSearch(2))
    mgobench.problem.coco.CocoSuite.testSuiteOptim("bbob-biobj",optimize.RandomSearch(2))
  }

}
