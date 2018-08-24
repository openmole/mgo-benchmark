package mgobench.problem.noise

case class GaussianNoise1D(mu : Double,sigma: Double, seed : Int) extends Noise {
  def noise(x : Vector[Double]) : Vector[Double] = {
    val rng = new util.Random(seed)
    Vector.fill(x.size)(sigma * rng.nextGaussian() + mu)
  }
}
