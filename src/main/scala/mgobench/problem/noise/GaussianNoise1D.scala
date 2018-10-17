package mgobench.problem.noise

import util.Random

case class GaussianNoise1D(mu : Double,sigma: Double, rng: Random) extends Noise {

  override def noise(x : Vector[Double]) : Vector[Double] = {
    Vector.fill(x.size)(sigma * rng.nextGaussian() + mu)
  }

  override def noiseName: String = this.toString

}
