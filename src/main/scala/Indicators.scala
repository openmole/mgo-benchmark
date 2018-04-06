import mgo.C

type Optimization = (Fitness,Vector[C]) => Result

type Fitness = Vector[Double] => Vector[Double]

type Result = Vector[(Vector[Double],Vector[Double])]



object Indicators {

  def expectedRunTime(problem : Problem, optimization : Optimization) : Double = {
    0.0
  }


}

