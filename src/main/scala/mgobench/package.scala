import mgobench.optimize.{GradientDescent, RandomSearch}
import mgobench.problem.coco.CocoSuite


package object mgobench {



  def testGradientDescent(iterations: Int) = {
    //Suite.testSuiteOptim("bbob",GradientDescent(iterations))
    val res = Benchmark.benchmark(Seq(GradientDescent(iterations),RandomSearch(iterations)),CocoSuite.getSuite("bbob"))
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
