package mgobench.problem.coco

import mgobench.optimize.Optimization
import mgobench.problem.noise.Noise
import mgobench.problem.{Problem, Suite}
import mgobench.utils.CocoJNI

case class CocoSuite(
                  pointer:Long,
                  name:String//,
                  //number_of_problems: Int = 1
                ) extends Suite {

  override def getNextProblem: Problem = CocoSuite.getNextProblem(this)

  override def reinitialize: Suite = CocoSuite.getSuite(name)

}





object CocoSuite {

  //construct a unique coco object
  lazy val coco: CocoJNI = {
    //System.loadLibrary("mgobench.utils.CocoJNI")
    //println(ClassLoader.getSystemClassLoader())
    //println(ClassLoader.class.("loadedLibraryNames").get(ClassLoader.getSystemClassLoader()))
    val coco = new CocoJNI
    coco.cocoSetLogLevel("info")
    coco
  }

  lazy val observer: Long = {
    val observerOptions = "result_folder: res"
    coco.cocoGetObserver("obs",observerOptions)
  }

  def apply(coco: CocoJNI, suiteName: String, suiteInstance: String, suiteOptions: String) = {
    println("creating suite " + suiteName)
    // set empty observer
    //coco.cocoGetObserver("no_observer", "")
    new CocoSuite(coco.cocoGetSuite(suiteName, suiteInstance, suiteOptions), suiteName)
  }




  /**
  *  Get a suite from its name : "bbob", "bbob-biobj"
   */
  def getSuite(name: String): CocoSuite = {
    // parameters example :
    //"instances: 10-20", "dimensions: 2,3,5,10,20 instance_indices:1-5")
    CocoSuite.apply(coco,name, "","")
  }





  /**
    *  Get next problem
    */
  def getNextProblem(suite: CocoSuite): CocoProblem = {
    // get observer pointer
    //val observer = coco.cocoGetObserver("no_observer","")
    CocoProblem(coco,coco.cocoSuiteGetNextProblem(suite.pointer,observer))
  }



  def problemNames(name: String) = {

    val suite: CocoSuite = CocoSuite.getSuite("bbob-biobj")
    println("suite ok")
    //"instances: 10-20", "dimensions: 2,3,5,10,20 instance_indices:1-5")
    var problem = CocoSuite.getNextProblem(suite)
    while (problem != CocoProblem.emptyProblem){
      println(problem.name)
      problem = CocoSuite.getNextProblem(suite)
    }

  }



  /**
    * testing
    */
  def testSuiteOptim(name: String, optimizer: Optimization) = {
    //val suite = mgobench.problem.coco.Suite(coco,name,"","")
    val suite = getSuite(name)

    var problem = CocoSuite.getNextProblem(suite)
    //while (problem != mgobench.problem.coco.CocoProblem.emptyProblem){
    for {_ <- 1 to 20} {
      println("\nProblem : "+problem.name)
      println("Boundaries : "+problem.boundaries)
      //println(mgobench.problem.coco.CocoProblem.evaluateFunction(coco,problem)(Vector.fill(problem.dimension)(0.0)))
      //println("Best solution : "+mgobench.optimize.RandomSearch.optimize(problem)(10000))
      //println("Fvalinterest : "+mgobench.problem.Problem.getLargestFValuesOfInterest(problem))

      println("Best solution : "+optimizer.optimize(problem))

      problem = getNextProblem(suite)
    }

    finalizeSuite(suite)

  }

  //def firstFitness(x: Vector[Double]): Vector[Double] = mgobench.problem.Problem.evaluateFunction(coco,firstProblem,x)
  //println(firstFitness(Vector.fill(firstProblem.dimension)(0.0)))
  //val boundaries = mgobench.problem.Problem.getBoundaries(firstProblem)




  /**
    * Finalizes the suite
    */
  def finalizeSuite(suite: CocoSuite): Unit = {
    coco.cocoFinalizeObserver(observer)
    coco.cocoFinalizeSuite(suite.pointer)
  }


}


// not needed for now
/*
class SuiteIterator(coco : mgobench.utils.CocoJNI,suite : mgobench.problem.coco.Suite) extends Iterator[mgobench.problem.Problem] {



  //def apply(c:mgobench.utils.CocoJNI,s:mgobench.problem.coco.Suite){new SuiteIterator(c,s)}

  // will not work as we rely on the side effects of the native library -> dirtily test in output
  //override def hasNext: Boolean = mgobench.problem.coco.Suite.getNextProblem(coco,suite) == mgobench.problem.Problem.emptyProblem
  override def hasNext: Boolean = true

  override def next(): mgobench.problem.Problem = mgobench.problem.coco.Suite.getNextProblem(coco,suite)

}
*/

