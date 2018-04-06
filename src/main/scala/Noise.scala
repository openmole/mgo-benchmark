



trait Noise {

  /**
    * Noise is generically non stationary
    * @param x
    * @return
    */
  def noise(x : Vector[Double]) : Vector[Double] = Vector.fill(x.size)(0.0)

}



case class GaussianNoise1D(mu : Double,sigma: Double)


object GaussianNoise1D extends Noise {

  //def apply(mu: Double = 0.0, sigma: Double = 0.01) : GaussianNoise = GaussianNoise(mu,sigma)

  def noise(x : Vector[Double])(noise: GaussianNoise1D)(rng : util.Random) : Vector[Double] = Vector.fill(x.size)((noise.sigma * rng.nextGaussian()) + noise.mu)

}

/**
  * Multi-dimensional gaussian noise
  * (independent)
  * @param mu
  * @param sigma
  */
case class GaussianNoiseIndep(mu : Vector[Double],sigma : Vector[Double])


object GaussianNoiseIndep extends Noise {

  def noise(x : Vector[Double],noise: GaussianNoiseIndep,rng : util.Random) : Vector[Double] = (noise.mu zip noise.sigma).map {
    case (m,s) => s*rng.nextGaussian() + m
  }

}



case class GammaNoise()


object GammaNoise extends Noise {

  override def noise(x : Vector[Double]) : Vector[Double] = noise(x,new GammaNoise,new util.Random)

  def noise(x : Vector[Double],noise: GammaNoise,rng : util.Random) : Vector[Double] = Vector.fill(x.size)(0.0)

}



case class NonStationaryGaussianNoise(
                                     dimension : Int,
                                     method : String,
                                     parameters : Vector[Double]
                                     )


object NonStationaryGaussianNoise extends Noise {

  // TODO : option how to vary noise parameters :
  //  - smoothed random fields ?
  //  - totally random ?
  //  - monotonous function ?

  //override def noise(x : Vector[Double]) : Vector[Double] = Vector.fill(x.size)(0.0)

  def getParams(noise : NonStationaryGaussianNoise)(implicit rng : util.Random) : (Vector[Double],Vector[Double]) = {
    noise.method match {
      case "random" => (Vector.fill(noise.dimension)(0.0),Vector.fill(noise.dimension)(rng.nextDouble()*noise.parameters(0)))
      case _ => (Vector.fill(noise.dimension)(0.0),Vector.fill(noise.dimension)(0.1))
    }
  }

  def noise(x : Vector[Double],noise : NonStationaryGaussianNoise)(implicit rng : util.Random) : Vector[Double] = {
    val params = getParams(noise)
    GaussianNoiseIndep.noise(x,GaussianNoiseIndep(params._1,params._2),rng)
  }


}


/**
  * Generic non stationary noise
  * @param dimension
  * @param method
  * @param parameters
  */
case class NonStationaryNoise(
                             dimension : Int,
                             method : String,
                             parameters : Vector[Double]
                             )


object NonStationaryNoise extends Noise {



}






