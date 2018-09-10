package mgobench


object Run extends App {

  val iterations = 1000

  mgobench.problem.coco.CocoSolutions.testResultExtraction()
  //testGradientDescent(iterations)
  //testCocoIntegration()
  //testGAOptim()


}