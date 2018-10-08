import mgobench.optimize._
import mgobench.problem.coco.CocoSuite


package object mgobench {


  def resultExtraction() = {
    //mgobench.problem.coco.CocoSolutions.testResultExtraction()
    mgobench.problem.coco.CocoSolutions.runResultExtraction()
  }



  def testGradientDescent(iterations: Int) = {
    //Suite.testSuiteOptim("bbob",GradientDescent(iterations))
    val res = Benchmark.benchmark(Seq(GradientDescent(iterations),mgobench.optimize.RandomSearch(iterations)),nBootstraps = 1,CocoSuite.getSuite("bbob"))
    println(res)
  }


  def testGAOptim() = {
    // GA optim
    //val results = (0 until 100).map(GA.replication.replication)
    //val deterministic = GA.replication.replication(0)(Rosenbrock.rosenbrock.apply,Rosenbrock.rosenbrock.genome(2))
    //println(deterministic.mkString("\n"))
    //println(deterministic)
    //println(Rosenbrock.counter)
  }

  def testCocoIntegration() = {
    // test coco integration
    //mgobench.problem.coco.CocoSuite.testSuiteOptim("bbob",optimize.RandomSearch(2))
    mgobench.problem.coco.CocoSuite.testSuiteOptim("bbob-biobj",optimize.RandomSearch(2))
  }

}
