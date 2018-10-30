
package mgobench.problem.coco

import mgobench.problem.coco.CocoSuite.{coco, observer}
import mgobench.problem.{Problem, Suite}
import mgobench.problem.noise.Noise

case class NoisyCocoSuite(
                           cocoSuite: CocoSuite,
                           noise: Noise
                         ) extends Suite {

  override def getNextProblem: Problem = NoisyCocoSuite.getNextProblem(cocoSuite,noise)

  override def reinitialize: Suite = NoisyCocoSuite(CocoSuite.getSuite(cocoSuite.name),noise)
}


object NoisyCocoSuite {

  /**
    * noisy suite from the name of the corresponding suite
    * @param name
    * @param noise
    * @return
    */
  def apply(name: String,noise: Noise): NoisyCocoSuite = NoisyCocoSuite(CocoSuite.getSuite(name),noise)

  def getNextProblem(suite: CocoSuite,noise: Noise): CocoProblem = CocoProblem(coco,coco.cocoSuiteGetNextProblem(suite.pointer,observer),noise)



}