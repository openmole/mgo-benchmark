package mgobench

package object utils {


  implicit class Ebesum(v: Vector[Double]){
    def +(v2: Vector[Double]): Vector[Double] = v.zip(v2).map{case(x1,x2)=>x1+x2}
  }


}
