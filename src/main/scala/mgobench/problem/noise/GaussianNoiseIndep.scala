package mgobench.problem.noise

/**
  * Multi-dimensional gaussian noise
  * (independent)
  *
  * @param mu
  * @param sigma
  */
case class GaussianNoiseIndep(mu : Vector[Double],sigma : Vector[Double], seed: Int) extends Noise {
  def noise(x : Vector[Double]) : Vector[Double] = {
    val rng = new util.Random(seed)
    (mu zip sigma).map {
      case (m,s) => s*rng.nextGaussian() + m
    }
  }
}

object GaussianNoiseIndep {

}


