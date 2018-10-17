package mgobench.problem.noise

import scala.util.Random

/**
  * Multi-dimensional gaussian noise
  * (independent)
  *
  * @param mu
  * @param sigma
  */
case class GaussianNoiseIndep(mu : Vector[Double],sigma : Vector[Double], rng: Random) extends Noise {
  def noise(x : Vector[Double]) : Vector[Double] = {
    (mu zip sigma).map {
      case (m,s) => s*rng.nextGaussian() + m
    }
  }

  override def noiseName: String = "Gaussian"
}

object GaussianNoiseIndep {

}


