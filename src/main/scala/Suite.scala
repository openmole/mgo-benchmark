
case class Suite(
                pointer: Long,
                name: String
                )

object Suite {

  def apply(coco: CocoJNI, suiteName: String, suiteInstance: String, suiteOptions: String) = {
    println("creating suite " + suiteName)
    new Suite(coco.cocoGetSuite(suiteName, suiteInstance, suiteOptions), suiteName)
  }

  /**
    * Finalizes the suite.
    * @throws Exception
    */
  def finalizeSuite(coco: CocoJNI, suite: Suite): Unit = {
    coco.cocoFinalizeSuite(suite.pointer)
  }


  /**
    *  Get next problem
   */

  def getNextProblem(coco: CocoJNI,suite: Suite): Problem = {
    // get observer pointer
    val observer = coco.cocoGetObserver("","")
    Problem(coco,coco.cocoSuiteGetNextProblem(suite.pointer,observer))
  }


}