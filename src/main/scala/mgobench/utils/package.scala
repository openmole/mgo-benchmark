package mgobench

package object utils {


  object implicits {

    /**
      * Decorator double vector operations
      * @param v
      */
    implicit class VectorDecorator(v: Vector[Double]) {
      //EbeSumDecorator
      def +(v2: Vector[Double]): Vector[Double] = v.zip(v2).map { case (x1, x2) => x1 + x2 }

      // EBEMinusDecorator
      def -(v2: Vector[Double]): Vector[Double] = v.zip(v2).map{case (x1,x2) => x1 - x2}

      // RightScalarMultiplicationDecorator
      def *(alpha: Double): Vector[Double] = v.map(_*alpha)

      // ScalarProductDecorator
      def x(v2: Vector[Double]): Double = v.zip(v2).map{case (x1,x2) => x1*x2}.sum

      def norm: Double = math.sqrt(v.map{x => x*x}.sum)
    }


    implicit class LeftScalarMultiplicationDecorator(alpha: Double) {
      def *(v: Vector[Double]): Vector[Double] = v.map{_*alpha}
    }

    /**
      * Matrix decorator using math3 commons -> useful ? (multiple conversions will deteriorate performances
      * @param m
      */
    implicit class MatrixDecorator(m: Vector[Vector[Double]]) {

    }



  }

  import implicits._

  def norm(v: Vector[Double]): Double = v.norm

  def ebesum(v1: Vector[Double], v2: Vector[Double]): Vector[Double] = v1 + v2

  def sd(x: Vector[Double]): Double = {
    val m = x.sum/x.size
    math.sqrt(x.map{case xx => (xx - m)*(xx - m)}.sum/x.size)
  }

  /**
    * Simple average aggregation
    * @param h
    * @return
    */
  def aggregation(h: Vector[Vector[Double]]): Vector[Double] = {
    //def sumvec(v1: Vector[Double],v2: Vector[Double]): Vector[Double] = v1.zip(v2).map{case (x1,x2)=>x1+x2}
    h.map{_.map{ _ / h.length}}.reduce(ebesum)
  }

  /**
    * Aggregation taking into variability with a simple gaussian CI
    *   - how the CI is computed should be adaptative ? bootstrap CI ? estimate noise distribution shape ?
    * @param h
    * @return
    */
  def aggregationWithCI(h: Vector[Vector[Double]]): Vector[Double] = {
    val sigmas = h.transpose.map(sd)
    val avgfitness = h.map{_.map{ _ / h.length}}.reduce(ebesum)
    //println(avgfitness.toString+"\n"+"+"+sigmas.toString)
    avgfitness.zip(sigmas).map{case(f,s)=> f + (1.96*s)/math.sqrt(h.length)}
  }



}

