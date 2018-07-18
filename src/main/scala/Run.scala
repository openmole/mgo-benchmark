

object Run extends App {

  val iterations = 10000



  // test coco integration
  //Suite.testSuiteOptim("bbob-biobj")
  Suite.testSuiteOptim("bbob",GradientDescent(iterations))

  // GA optim
  //val results = (0 until 100).map(GA.replication.replication)
  //val deterministic = GA.replication.replication(0)(Rosenbrock.rosenbrock.apply,Rosenbrock.rosenbrock.genome(2))
  //println(deterministic.mkString("\n"))
  //println(deterministic)
  //println(Rosenbrock.counter)


}