package mgobench.problem.noise

trait Noise {

  /**
    * Noise is generically non stationary
    * @param x
    * @return
    */
  def noise(x : Vector[Double]) : Vector[Double]

}
