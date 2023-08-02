package mgobench.test

import mgo.evolution.C
import mgobench.{Benchmark, optimise}
import mgobench.optimise.{GradientDescent, Optimisation}
import mgobench.optimise.ga.*
import mgobench.optimise.pso.GCPSO
import mgobench.problem.FitnessSuite
import mgobench.problem.coco.{CocoProblem, CocoSolutions, CocoSuite, NoisyCocoSuite}
import mgobench.problem.noise.GaussianNoise1D
import mgobench.result.{Indicators, Result}


object Test extends App {



  //test.testNSGA3ReferencePoints
  test.testNSGA3()


}




package object test {


  def testBenchmark(): Unit = {
    println("Running test benchmark")
    val iterations = 10000
    val nrepets = 10
    val sigma = 2.0
    val res: Seq[Result] = Benchmark.benchmark(
      optimizers = Seq(
        mgobench.optimise.RandomSearch(iterations / nrepets,nrepets,1),
        mgobench.optimise.GradientDescent(iterations / nrepets, nrepets),
        mgobench.optimise.NoisyGradientDescent(iterations=iterations/(100*nrepets),stochastic_iterations=nrepets,nsearchs=100,tolerance=1e-20),
        mgobench.optimise.ga.NSGA2Optimisation(lambda = 100, mu = 20,nrepets = 1,generations = (iterations/100) - 1),
        mgobench.optimise.ga.KalmanNSGA2(lambda = 100, mu = 20, generations = (iterations/100)-1, cloneProbability = 0.5,observationNoise = 1.0),
        mgobench.optimise.ga.NoisyNSGA2Optimisation(lambda=100, mu = 20,generations = (iterations/100)-1,historySize = 100,cloneProbability = 0.2),
        mgobench.optimise.pso.GlobalBestPSO(iterations = iterations / 100,particles = 100)
      ),
      nBootstraps = 1,
      suite = NoisyCocoSuite("bbob",GaussianNoise1D(0,sigma,1)),
      problemsNumber = 1,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )

    println(res.mkString("\n"))
    //utils.io.File.writeCSV(Indicators.computeExpectedIndicators(res),"res/test/test.csv",";")

  }


  def testNSGA3(): Unit = {
    println("Test NSGA3")

    //def f(x: Vector[Double]): Vector[Double] = Vector(x(0)+x(1),1/(1+x(0))+x(1))
    // -> gives singular rotation matrices (expected as min at x = y for first objective ?)
    def f(x: Vector[Double]): Vector[Double] = Vector(x(0),1/x(0))

    //val b: Vector[C] = Vector(C(0.0,1.0),C(0.0,1.0))
    val b: Vector[C] = Vector(C(0.1,10.0))

    val iterations = 10000
    val res = Benchmark.benchmark(Seq(
      mgobench.optimise.ga.NSGA3Optimisation(
        popSize = 100,
        generations = iterations,
        referencePoints = AutoReferences(90))
      ),
      nBootstraps = 1,
      suite = FitnessSuite(f,b)
    )

    println(res.mkString("\n"))
  }


  /**
    * test generation of simplex points
    */
  def testNSGA3ReferencePoints(): Unit = {
    // first check : number of points

    import org.apache.commons.math3.util._

    // dimension + divisions
    val res = (for {
      dim <- 2 to 10 by 1
      p <- 1 to 5 by 1
      expectedNumber = CombinatoricsUtils.binomialCoefficient(dim + p - 1,p)
      points: Vector[Vector[Double]] = NSGA3Optimisation.simplexRefPoints(p,dim)
    } yield {
      (dim,p)->(expectedNumber,points.size,points)
      //(dim,p)->(expectedNumber,points.size)
    })
    val resMap = res.toMap

    //println(res)
    println("Cum error = "+res.map{r => math.abs(r._2._1.toInt - r._2._2)}.sum)
    println("Num of errors = "+res.filter(r => r._2._1.toInt != r._2._2).size)

    println(res.filter(r => r._2._1 != r._2._2).map(r=> (r._1,r._2._1,r._2._2)))

    //println(res.filter(_._1._1==3))
    //println(res((10,5)))
    //println(res(2,1))
    // failing for p=4, dim >= 4 ?
    //println(res(5,6))
    //import mgobench.utils.implicits._
    //println(res(4,4)._3.reduce(_+_))

    println(resMap((10,5)))
    //println(resMap((3,2)))
    //println(containsBasis(resMap((4,4))._3,4))
    //println(containsAxis(resMap((4,4))._3,4,4))

    //println(missingPoints(resMap((4,4))._3,axis(4,4)))
    //println(axis(4,4).size)

  }

  def containsPoints(points: Vector[Vector[Double]],tocontain: Vector[Vector[Double]]): Boolean = tocontain.map(points.contains(_)).reduce(_&_)
  def missingPoints(points: Vector[Vector[Double]],tocontain: Vector[Vector[Double]]): Vector[Vector[Double]] = tocontain.filter(!points.contains(_))

  def containsBasis(points: Vector[Vector[Double]],dimension: Int): Boolean = {
    val basis = Vector.tabulate(dimension){i => Point(Vector.tabulate(dimension){j => if (j==i) Fraction(1) else Fraction(0)})}
    containsPoints(points,basis.map(_.toDoubleVector))
  }

  def axis(dimension: Int,divisions: Int): Vector[Vector[Double]] = Vector.tabulate(dimension){i => (1.0 to (divisions - 1) by 1.0).toVector.map{k => Vector.tabulate(dimension){j => if (j==i) k/divisions  else 0.0}}}.reduce(_++_)

  def containsAxis(points: Vector[Vector[Double]],dimension: Int, divisions: Int): Boolean = {
    // axis without extremities :
     containsPoints(points,axis(dimension,divisions))
  }



  def testNoisyGA(): Unit = {
    println("Running test NoisyNSGA2")
    val iterations = 10000
    val sigma = 2.0
    val res: Seq[Result] = Benchmark.benchmark(
      optimizers = Seq(
        mgobench.optimise.ga.NoisyNSGA2Optimisation(lambda=100, mu = 20,generations = (iterations/100)-1,historySize = 100,cloneProbability = 0.2).asInstanceOf[Optimisation]
      ),
      nBootstraps = 1,
      suite = NoisyCocoSuite("bbob",GaussianNoise1D(0,sigma,1)),
      problemsNumber = 1,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )

    println(res.mkString("\n"))
  }

  def testKalmanGA(): Unit = {
    val iterations = 5000
    val res: Seq[Result] = Benchmark.benchmark(
      optimizers = Seq(
        KalmanNSGA2(lambda = 100, mu = 20, generations = iterations-1, cloneProbability = 0.5,observationNoise = 1.0)
      ),
      nBootstraps = 1,
      suite = CocoSuite.getSuite("bbob"),
      problemsNumber = 1,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )
    val hist = CocoSolutions.loadSolutions("data/historicalresults.csv")
    println(res)
    println(Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(0.01,hist),res.toVector))
  }

  def testPSO(): Unit = {
    val iterations = 1000
    val res: Seq[Result] = Benchmark.benchmark(
      optimizers = Seq(
        //BasicPSOAkka(iterations,10) // akka does not work with coco for parallelization
        //GlobalBestPSO(iterations,200)
        GCPSO(iterations,10)
      ),
      nBootstraps = 10,
      suite = CocoSuite.getSuite("bbob"),
      problemsNumber = 2,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )
    val hist = CocoSolutions.loadSolutions("data/historicalresults.csv")
    println(Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(0.01,hist),res.toVector))
  }


  def testGradientDescent(): Unit = {
    val iterations = 100

    //Suite.testSuiteOptim("bbob",GradientDescent(iterations))
    val res = Benchmark.benchmark(Seq(GradientDescent(
      iterations
      ,tolerance = 1e-30
    )),
      nBootstraps = 1,CocoSuite.getSuite("bbob"),problemsNumber = 1,
      problemFilter = _.asInstanceOf[CocoProblem].instance <= 5
    )
    //println(res)
    val hist = CocoSolutions.loadSolutions("data/historicalresults.csv")
    println(Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(0.001,hist),res.toVector))
  }

  def testRandomSearch(iterations: Int): Unit = {
    //Suite.testSuiteOptim("bbob",GradientDescent(iterations))
    val res = Benchmark.benchmark(Seq(mgobench.optimise.RandomSearch(iterations)),
      nBootstraps = 2,CocoSuite.getSuite("bbob"),problemsNumber = 15*24+2)
    //println(res)
    //val hist = CocoSolutions.loadSolutions("data/historicalresults.csv")
    //println(Indicators.expectedRunTime(Indicators.historicalSolutionSuccess(10.0,hist),res.toVector))
  }



  def testGAOptim(): Unit = {
    // GA optim
    //val results = (0 until 100).map(GA.replication.replication)
    //val deterministic = GA.replication.replication(0)(Rosenbrock.rosenbrock.apply,Rosenbrock.rosenbrock.genome(2))
    //println(deterministic.mkString("\n"))
    //println(deterministic)
    //println(Rosenbrock.counter)
  }

  def resultExtraction(): Unit = {
    //mgobench.problem.coco.CocoSolutions.testResultExtraction()
    mgobench.problem.coco.CocoSolutions.runResultExtraction()
  }

  def testCocoIntegration(): Unit = {
    // test coco integration
    //mgobench.problem.coco.CocoSuite.testSuiteOptim("bbob",optimize.RandomSearch(2))
    mgobench.problem.coco.CocoSuite.testSuiteOptim("bbob-biobj",optimise.RandomSearch(2))
  }

}


