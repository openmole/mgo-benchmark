



trait Noise {

  /**
    * Noise is generically non stationary
    * @param x
    * @return
    */
  def noise(x : Vector[Double]) : Vector[Double] = Vector.fill(x.size)(0.0)

}



case class GaussianNoise(mu : Double,sigma: Double)


object GaussianNoise extends Noise {

  //def apply(mu: Double = 0.0, sigma: Double = 0.01) : GaussianNoise = GaussianNoise(mu,sigma)

  def noise(x : Vector[Double])(noise: GaussianNoise)(rng : util.Random) : Vector[Double] = Vector.fill(x.size)((noise.sigma * rng.nextGaussian()) + noise.mu)

}


case class GammaNoise()


object GammaNoise extends Noise {


  def noise(x : Vector[Double])(noise: GammaNoise)(rng : util.Random) : Vector[Double] = Vector.fill(x.size)(0.0)

}



case class NonStationaryGaussianNoise()


object NonStationaryGaussianNoise extends Noise {

  // TODO : option how to vary noise parameters :
  //  - smoothed random fields ?
  //  - totally random ?
  //  - monotonous function ?

  def noise(x : Vector[Double]) : Vector[Double] = Vector.fill(x.size)(0.0)

}










