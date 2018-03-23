
/*case class Suite(
                pointer: Long,
                name: String
                )
*/

class Suite(p:Long,name:String,coco:CocoJNI) extends Iterable[Problem] {

  def pointer : Long = p

  /*
  def apply(coco: CocoJNI, suiteName: String, suiteInstance: String, suiteOptions: String) = {
    println("creating suite " + suiteName)
    // set empty observer
    coco.cocoGetObserver("no_observer", "")
    new Suite(coco.cocoGetSuite(suiteName, suiteInstance, suiteOptions), suiteName)
  }
  */
  def this(coco: CocoJNI, suiteName: String, suiteInstance: String, suiteOptions: String) =
    this(coco.cocoGetSuite(suiteName, suiteInstance, suiteOptions), suiteName,coco)




  override def iterator: Iterator[Problem] = new SuiteIterator(this.coco,this)


}


object Suite {


  /**
    *  Get next problem
    */

  def getNextProblem(coco: CocoJNI,suite: Suite): Problem = {
    // get observer pointer
    val observer = coco.cocoGetObserver("no_observer","")
    Problem(coco,coco.cocoSuiteGetNextProblem(suite.pointer,observer))
  }


  /**
    * Finalizes the suite.
    * @throws Exception
    */
  def finalizeSuite(coco: CocoJNI, suite: Suite): Unit = {
    coco.cocoFinalizeSuite(suite.pointer)
  }

}


class SuiteIterator(coco : CocoJNI,suite : Suite) extends Iterator[Problem] {



  //def apply(c:CocoJNI,s:Suite){new SuiteIterator(c,s)}

  // will not work as we rely on the side effects of the native library -> dirtily test in output
  //override def hasNext: Boolean = Suite.getNextProblem(coco,suite) == Problem.emptyProblem
  override def hasNext: Boolean = true

  override def next(): Problem = Suite.getNextProblem(coco,suite)

}


