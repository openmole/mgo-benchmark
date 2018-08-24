package mgobench.problem



trait Suite{

  /**
    * Iterate over problems
    *
    * @return
    */
  def getNextProblem: Problem

  /**
    * cloning
    * @return
    */
  def reinitialize: Suite


  // can not monitor current progress with this shitty native lib
  //def hasNext: Boolean

}



