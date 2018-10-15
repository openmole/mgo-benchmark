package mgobench.result

import mgobench.optimize.Optimization
import mgobench.problem.Problem
import mgobench.problem.coco.{CocoProblem, HistoricalSolution}










object Indicators {

  /*
  // bootstrap should be explicited in benchmark, not in indicators
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
  */

  /**
    * Compare solution with best historical
    *  - 1 obj only
    * @param epsilon
    * @return
    */
  def historicalSolutionSuccess(epsilon: Double,hist: scala.collection.Map[(String,Int),HistoricalSolution]): Result => Boolean = {
    result => {
      val pb = result.problem.asInstanceOf[CocoProblem]
      val id = pb.id
      //println(id)
      val fitval = result.values(0)(0)
      val key = id.split("_")(1).replaceAll("0","")+"_"+id.split("_")(2).replaceAll("i","").toInt.toString
      val dim = result.problem.asInstanceOf[CocoProblem].dimension
      val histfitval = if(hist.keySet.contains((key,dim))) hist((key,dim)).bestFitnesses(0)(0) else fitval//10000.0
      // fail if no historical value ? -> ensure all solutions using an algo with numerous iterations / simulated annealing ?
      //println(id+" : "+histfitval+" - "+fitval+" : "+(math.abs(fitval - histfitval) < epsilon))
      math.abs(fitval - histfitval) < epsilon
    }
  }


  /**
    * Generalization of expected runtime
    * @param indic
    * @param successCondition
    * @param results
    * @return
    */
  def expectedIndicator(indic : Result => Double)(successCondition: Result => Boolean)(results : Vector[Result]): Double = {
    val successes = results.map {case r => if (successCondition(r)){1.0} else {0.0}}
    val unsucc = results.map {case r => if (successCondition(r)){0.0} else {1.0}}
    println("successes = "+successes.sum+" ; fails = "+unsucc.sum)
    //println(results.map(indic))
    val ps = successes.sum / results.size
    if(ps > 0.0&&unsucc.sum > 0.0) {
      successes.zip(results).map { case (s, r) => s * indic(r) }.sum / successes.sum + (1 - ps) * (unsucc.zip(results).map { case (s, r) => s * indic(r) }.sum / unsucc.sum) / ps
    }else{
      0.0
    }
    }

  def expectedRunTime(successCondition: Result => Boolean,results : Vector[Result]): Double =
    expectedIndicator(_.runs)(successCondition)(results)

  def expectedPrecision(successCondition: Result => Boolean,results : Vector[Result]): Double =
    expectedIndicator(_.precision)(successCondition)(results)

}

