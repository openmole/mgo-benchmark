
import benchmark._

object Run extends App {

  System.loadLibrary("CocoJNI")

  System.loadLibrary("CocoJNI")
  val coco = new CocoJNI
  //coco.cocoSetLogLevel("info")
  val suite: Suite = new Suite(coco,"bbob-biobj", "","")
  println("suite ok")
  //"instances: 10-20", "dimensions: 2,3,5,10,20 instance_indices:1-5")
  var problem = Suite.getNextProblem(coco,suite)
  while (problem != CocoProblem.emptyProblem){
    println(problem.name)
    problem = Suite.getNextProblem(coco,suite)
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