package mgobench.problem

//import mgobench.problem.Suite
import mgobench.utils._

case class StandardSuite(name: String) extends Suite {

  override def getNextProblem: Problem = Problem.emptyProblem

  override def reinitialize: Suite = this

}



object StandardSuite {

  val functions: Seq[AnyRef] = Seq(Rosenbrock.rosenbrock,Rastrigin.rastrigin,Ellipsoidal.ellipsoidal,Ackley.ackley,Griewank.griewank,Langermann.langermann)

}



