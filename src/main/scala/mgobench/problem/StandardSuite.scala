package mgobench.problem

//import mgobench.problem.Suite
import mgobench.utils._

case class StandardSuite(name: String) extends Suite {

  override def getNextProblem: Problem = Problem.emptyProblem

  override def reinitialize: Suite = this

}



object StandardSuite {

  val functions = Seq(Rosenbrock.rosenbrock)
}



