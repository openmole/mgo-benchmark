

object CocoJNI {

  System.loadLibrary("CocoJNI")

  @native def cocoSetLogLevel (logLevel: String): Unit

  // Observer
  @native def cocoGetObserver(observerName: String, observerOptions: String): Long

  @native def cocoFinalizeObserver(observerPointer: Long): Unit

  @native def cocoProblemAddObserver(problemPointer: Long, observerPointer: Long): Long

  @native def cocoProblemRemoveObserver(problemPointer: Long, observerPointer: Long): Long

  // Suite
  @native def cocoGetSuite(suiteName: String, suiteInstance: String, suiteOptions: String): Long

  @native def cocoFinalizeSuite(suitePointer: Long): Unit

  // Problem
  @native def cocoSuiteGetNextProblem(suitePointer: Long, observerPointer: Long): Long

  @native def cocoSuiteGetProblem(suitePointer: Long, problemIndex: Long): Long

  // Functions
  @native def cocoEvaluateFunction(problemPointer: Long, x: Array[Double]): Array[Double]

  @native def cocoEvaluateConstraint(problemPointer: Long, x: Array[Double]): Array[Double]

  // Getters
  @native def cocoProblemGetDimension(problemPointer: Long): Int

  @native def cocoProblemGetNumberOfObjectives(problemPointer: Long): Int

  @native def cocoProblemGetNumberOfConstraints(problemPointer: Long): Int

  @native def cocoProblemGetSmallestValuesOfInterest(problemPointer: Long): Array[Double]

  @native def cocoProblemGetLargestValuesOfInterest(problemPointer: Long): Array[Double]

  @native def cocoProblemGetLargestFValuesOfInterest(problemPointer: Long): Array[Double]

  @native def cocoProblemGetId(problemPointer: Long): String

  @native def cocoProblemGetName(problemPointer: Long): String

  @native def cocoProblemGetEvaluations(problemPointer: Long): Long

  @native def cocoProblemGetEvaluationsConstraints(problemPointer: Long): Long

  @native def cocoProblemGetIndex(problemPointer: Long): Long

  @native def cocoProblemIsFinalTargetHit(problemPointer: Long): Int


}
