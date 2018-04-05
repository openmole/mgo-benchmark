

object Run extends App {

  val suite: Suite = Suite.getSuite("bbob-biobj")
  println("suite ok")
  //"instances: 10-20", "dimensions: 2,3,5,10,20 instance_indices:1-5")
  var problem = Suite.getNextProblem(suite)
  while (problem != CocoProblem.emptyProblem){
    println(problem.name)
    problem = Suite.getNextProblem(suite)
  }



  // test coco integration
  //Suite.testSuiteOptim("bbob-biobj")
  //Suite.testSuiteOptim(coco,"bbob")

  // GA optim
  //val results = (0 until 100).map(GA.replication.replication)
  //val deterministic = GA.replication.replication(0)(Rosenbrock.rosenbrock.apply,Rosenbrock.rosenbrock.genome(2))
  //println(deterministic.mkString("\n"))
  //println(deterministic)
  //println(Rosenbrock.counter)


}