

case class Suite(
                  pointer:Long,
                  name:String
                )



object Suite {

  //construct a unique coco object
  lazy val coco: CocoJNI = {
    //System.loadLibrary("CocoJNI")
    //println(ClassLoader.getSystemClassLoader())
    //println(ClassLoader.class.("loadedLibraryNames").get(ClassLoader.getSystemClassLoader()))
    val coco = new CocoJNI
    coco.cocoSetLogLevel("info")
    coco
  }

  def apply(coco: CocoJNI, suiteName: String, suiteInstance: String, suiteOptions: String) = {
    println("creating suite " + suiteName)
    // set empty observer
    //coco.cocoGetObserver("no_observer", "")
    new Suite(coco.cocoGetSuite(suiteName, suiteInstance, suiteOptions), suiteName)
  }




  /**
  *  Get a suite from its name : "bbob", "bbob-biobj"
   */
  def getSuite(name: String): Suite = {
    // parameters example :
    //"instances: 10-20", "dimensions: 2,3,5,10,20 instance_indices:1-5")
    Suite.apply(coco,name, "","")
  }


  /**
    *  Get next problem
    */
  def getNextProblem(suite: Suite): CocoProblem = {
    // get observer pointer
    val observer = coco.cocoGetObserver("no_observer","")
    CocoProblem(coco,coco.cocoSuiteGetNextProblem(suite.pointer,observer))
  }


  /**
    * testing
    */
  def testSuiteOptim(name: String) = {
    val suite = Suite(coco,name,"","")

    var problem = Suite.getNextProblem(suite)
    while (problem != CocoProblem.emptyProblem){
      println("\nProblem : "+problem.name)
      println("Boundaries : "+problem.getBoundaries(problem))
      println(CocoProblem.evaluateFunction(coco,problem)(Vector.fill(problem.dimension)(0.0)))
      println("Best solution : "+RandomSearch.optimize(problem)(10000))
      //println("Fvalinterest : "+Problem.getLargestFValuesOfInterest(problem))
      problem = Suite.getNextProblem(suite)
    }
  }

  //def firstFitness(x: Vector[Double]): Vector[Double] = Problem.evaluateFunction(coco,firstProblem,x)
  //println(firstFitness(Vector.fill(firstProblem.dimension)(0.0)))
  //val boundaries = Problem.getBoundaries(firstProblem)



  /*
  /**
    * Finalizes the suite.
    * @throws Exception
    */
  def finalizeSuite(coco: CocoJNI, suite: Suite): Unit = {
    coco.cocoFinalizeSuite(suite.pointer)
  }
  */

}


// not needed for now
/*
class SuiteIterator(coco : CocoJNI,suite : Suite) extends Iterator[Problem] {



  //def apply(c:CocoJNI,s:Suite){new SuiteIterator(c,s)}

  // will not work as we rely on the side effects of the native library -> dirtily test in output
  //override def hasNext: Boolean = Suite.getNextProblem(coco,suite) == Problem.emptyProblem
  override def hasNext: Boolean = true

  override def next(): Problem = Suite.getNextProblem(coco,suite)

}
*/

