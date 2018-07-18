import mgo.C










object Indicators {

  def estimateIndicators(problem : Problem,
                         optimization : Optimization,
                         nBootstraps: Int,
                         indicators: Vector[Vector[Result] => Double]
                        ) : Vector[Double] = {
    val results :Vector[Result] = (1 until nBootstraps).to[Vector].map {_ =>
      optimization.optimize(problem)
    }
    indicators.map{_(results)}
  }

  def expectedIndicator(indic : Result => Double)(successCondition: Result => Boolean)(results : Vector[Result]): Double = {
    val successes = results.map {case r => if (successCondition(r)){1.0} else {0.0}}
    val unsucc = results.map {case r => if (successCondition(r)){0.0} else {1.0}}
    val ps = successes.sum / results.size
    successes.zip(results).map{case (s,r) => s*indic(r)}.sum / successes.sum + (1 - ps) * (unsucc.zip(results).map{case(s,r)=>s*indic(r)}.sum / unsucc.sum)/ps
  }

  def expectedRunTime(successCondition: Result => Boolean)(results : Vector[Result]): Double =
    expectedIndicator(_.runs)(successCondition)(results)

  def expectedPrecision(successCondition: Result => Boolean)(results : Vector[Result]): Double =
    expectedIndicator(_.precision)(successCondition)(results)

}

