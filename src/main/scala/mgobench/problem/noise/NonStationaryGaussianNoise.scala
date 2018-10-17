package mgobench.problem.noise

case class NonStationaryGaussianNoise(
                                     dimension : Int,
                                     method : String,
                                     parameters : Vector[Double]
                                     ) extends Noise {
  override def noise(x: Vector[Double]): Vector[Double] = NonStationaryGaussianNoise.noise(x,this)(new scala.util.Random)

  override def noiseName: String = "NonStationaryGaussian"

}

object NonStationaryGaussianNoise  {

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
    val gaussianNoise = GaussianNoiseIndep(params._1,params._2,rng)
    gaussianNoise.noise(x)
  }


}