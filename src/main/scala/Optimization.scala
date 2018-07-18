import mgo.C


/**
  * Optimization algorithm
  */
//type Optimization = (Fitness,Vector[C]) => Result


trait Optimization {

  def optimize(problem: Problem) : Result

}



