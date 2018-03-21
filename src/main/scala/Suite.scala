
case class Suite(
                pointer: Long,
                name: String
                )

object Suite {

  def apply(suiteName: String, suiteInstance: String, suiteOptions: String) = new Suite(
    CocoJNI.cocoGetSuite(suiteName, suiteInstance, suiteOptions),
    suiteName
  )


  /**
    * Finalizes the suite.
    * @throws Exception
    */
  def finalizeSuite(suite: Suite): Unit = {
    CocoJNI.cocoFinalizeSuite(suite.pointer)
  }

}