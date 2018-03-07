
import mgo._
import mgo.contexts._
import freedsl.dsl._

object Run extends App {

  object GaussianNoise {
    def apply(rng: util.Random)(mu: Double = 0.0, sigma: Double = 1.0)= (sigma * rng.nextGaussian()) + mu
  }


  def replication(seed: Int) = {
    val rng = new util.Random(seed)

    val nsga2 = NSGA2(
      mu = 20,
      lambda = 20,
      fitness = x => Vector(Rastrigin.rastrigin(x) + GaussianNoise(rng)()),
      continuous = Rastriigin.rastrigin.genome(2)
    )

    def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
      nsga2.
        until(afterGeneration(1000)).
        trace { (s, is) =>
          //println(s.generation)
          //println(NSGA2.result(nsga2, is).mkString("\n"))
        }.
        evolution


    val (finalState, finalPopulation) =
      NSGA2.run(rng) { imp =>
        import imp._
        evolution[DSL].eval
      }

    NSGA2.result(nsga2, finalPopulation).head.fitness.head
  }

  val results = (0 until 100).map(replication)
  println(results.mkString(" "))
  //println(NSGA2.result(nsga2, finalPopulation).mkString("\n"))


}