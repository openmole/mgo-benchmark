

import mgo._

import util.Random
import algorithm._
import mgo.algorithm.CDGenome.DeterministicIndividual.{Individual, vectorFitness}
import mgo.algorithm.CDGenome.{DeterministicIndividual, buildGenome}

object RandomSearch {


  /*def optimize(fitness : Vector[Double] => Vector[Double])(genome : Vector[C])(nsearchs : Int)(rng : Random) : Vector[(Vector[Double],Vector[Double])] = (1 until nsearchs).map { n =>
      val xvec = Vector.fill(genome.size)(rng.nextDouble)
      (fitness(xvec),xvec)
    }.to[Vector].sortWith((x,y) => x._1(0) < y._1(0))
  */

  def optimize(fitness : Vector[Double] => Vector[Double])(genome : Vector[C])(nsearchs: Int)(rng : Random) = {
    val points = Vector.fill(nsearchs)(genome.map {c => (c.high - c.low)*rng.nextDouble() +  c.low})
    paretoFront(points,fitness)
  }

  def paretoFront(points : Vector[Vector[Double]],fitness : Vector[Double]=>Vector[Double]): Vector[Individual] = {
    val population = points.map{x => DeterministicIndividual.buildIndividual(buildGenome(x, None, Vector.empty, None), fitness(x))}
    println(population)
    keepFirstFront(population, vectorFitness.get)
  }


}


