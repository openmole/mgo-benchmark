package mgobench.result

import mgobench.optimize.Optimization
import mgobench.problem.Problem
import mgobench.problem.coco.{CocoProblem, CocoSolutions, HistoricalSolution}
import sun.security.util.PropertyExpander.ExpandException










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
  def historicalSolutionSuccess(epsilon: Double,hist: scala.collection.Map[(String,Int),HistoricalSolution]): Result => (Boolean,Double,Double) = {
    result => {
      val pb = result.problem.asInstanceOf[CocoProblem]
      val id = pb.id
      //println(id)
      val fitval = result.values(0)(0)
      val key = id.split("_")(1).replaceAll("0","")+"_"+id.split("_")(2).replaceAll("i","").toInt.toString
      val dim = result.problem.asInstanceOf[CocoProblem].dimension
      assert(hist.keySet.contains((key,dim)),s"No historical solution for problem $key $dim")
      val histfitval = hist((key,dim)).bestFitnesses(0)(0)
      val histsol = hist((key,dim)).bestSolutions(0)
      // fail if no historical value ? -> ensure all solutions using an algo with numerous iterations / simulated annealing ?
      //println(id+" : "+histfitval+" - "+fitval+" : "+(math.abs(fitval - histfitval) < epsilon))
      ((math.abs(fitval - histfitval) < epsilon),fitval,histfitval)//&&(math.sqrt(histsol.zip(result.points(0)).map{case (x1,x2) => math.pow(x1-x2,2.0)}.sum)<epsilon)
    }
  }


  /**
    * Generalization of expected runtime
    * @param indic
    * @param successCondition
    * @param results
    * @return
    */
  def expectedIndicator(indic : Result => Double)(successCondition: Result => (Boolean,Double,Double))(results : Vector[Result]): ExpectedIndicator = {
    val successes = results.map {case r => if (successCondition(r)._1){1.0} else {0.0}}
    val unsucc = results.map {case r => if (successCondition(r)._1){0.0} else {1.0}}
    //println("successes = "+successes.sum+" ; fails = "+unsucc.sum)
    //println(results.map(indic))
    val hist = results.map(successCondition).map(_._3).min
    val best = results.map(successCondition).map(_._2).min
    val ps = successes.sum / results.size
    (ps,unsucc.sum ) match {
      case (ps,u) if ps > 0.0&&u > 0.0 => {
      val value = successes.zip(results).map { case (s, r) => s * indic(r) }.sum / successes.sum + (1 - ps) * (unsucc.zip(results).map { case (s, r) => s * indic(r) }.sum / unsucc.sum) / ps
      ExpectedIndicator(value,best,hist,successes.sum.toInt,unsucc.sum.toInt,indic.toString)
    }
      case (ps,u) if ps > 0.0&&u == 0.0 => {
        ExpectedIndicator(successes.zip(results).map { case (s, r) => s * indic(r) }.sum / successes.sum,best,hist,successes.sum.toInt,unsucc.sum.toInt,indic.toString)
      }
      case _ => ExpectedIndicator(Double.PositiveInfinity,best,hist,successes.sum.toInt,unsucc.sum.toInt,indic.toString)
    }
  }

  def expectedRunTime(successCondition: Result => (Boolean,Double,Double),results : Vector[Result]): ExpectedIndicator =
    expectedIndicator(_.runs)(successCondition)(results)

  def expectedPrecision(successCondition: Result => (Boolean,Double,Double),results : Vector[Result]): ExpectedIndicator =
    expectedIndicator(_.precision)(successCondition)(results)


  case class ExpectedIndicator(value: Double,best: Double,hist: Double,successes: Int,failures: Double,name: String)

  def computeExpectedIndicators(res: Seq[Result],historicalResultsFile: String = "data/historicalresults.csv"):Array[Array[Any]] = {
    val hist = CocoSolutions.loadSolutions(historicalResultsFile)
    res.groupBy(_.id).map {
      case (k,results) =>
        //println("Expected runtimes for "+k+" : " + Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(0.1, hist), results.toVector))
        Array(k,expectedRunTime(historicalSolutionSuccess(0.1, hist), results.toVector).value,expectedPrecision(historicalSolutionSuccess(0.1, hist), results.toVector).value)
    }.toArray
      //.toList.sorted.mkString("\n")
  }


}

