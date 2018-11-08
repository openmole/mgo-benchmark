package mgobench.optimize

import cats.data._
import cats.implicits._
import mgo.ranking._
import shapeless.Lazy

package object ga {

  /**
    * Combine two rankings to be used in a lexical order in a tournament
    *
    * @param ranking1
    * @param ranking2
    * @tparam M
    * @tparam I
    * @return
    */
  def lexicoRanking[M[_]: cats.Monad, I](ranking1: Ranking[M, I],ranking2: Ranking[M, I]): Kleisli[M, Vector[I], Vector[(Lazy[Int], Lazy[Int])]] =
    Kleisli((population: Vector[I]) =>
      for {
        r1 <- ranking1(population)
        r2 <- ranking2(population)
      } yield r1 zip r2)

  def acceptableRanking[M[_]: cats.Monad, I](fitness: I => Vector[Double],acceptableFitness: Vector[Double]): Kleisli[M,Vector[I],Vector[Lazy[Int]]] = {
    def acceptableComps = (i: I) => fitness(i).zip(acceptableFitness).map{case(f,fa)=>if(f<fa)0.0 else 1.0}
    paretoRanking[M,I](acceptableComps)
  }


}
