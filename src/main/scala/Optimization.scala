import mgo.C


/**
  * Optimization algorithm
  */
//type Optimization = (Fitness,Vector[C]) => Result


trait Optimization {

  def optimize(fitness: Vector[Double] => Vector[Double],bounds : Vector[C]) : Result

}



