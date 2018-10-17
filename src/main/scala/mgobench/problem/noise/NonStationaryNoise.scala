package mgobench.problem.noise






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
                             ) extends Noise {

  override def noise(x: Vector[Double]): Vector[Double] = Vector.fill(dimension)(0.0)

  override def noiseName: String = "NonStationaryNoise"

}


object NonStationaryNoise  {

}



/*
case class GammaNoise()


object GammaNoise extends Noise {

  override def noise(x : Vector[Double]) : Vector[Double] = noise(x,new GammaNoise,new util.Random)

  def noise(x : Vector[Double],noise: GammaNoise,rng : util.Random) : Vector[Double] = Vector.fill(x.size)(0.0)

}*/


