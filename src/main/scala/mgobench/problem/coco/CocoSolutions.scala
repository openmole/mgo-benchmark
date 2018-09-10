
package mgobench.problem.coco

import java.io.File

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io._
import scala.sys.process._

object CocoSolutions {

  /**
    * Loading for 1d
    * @param f
    * @return
    */
  def dataload1dimension(f:File,sep: String = " "): HistoricalSolution ={
    println("Loading data file "+f.toString)
    val solutions = ArrayBuffer[HistoricalSolution]()
    // get function name and dimension from file name
    for(l <- Source.fromFile(f).getLines()){
      if(!l.startsWith("%")) {// comments
        val values: Array[Double] = l.split(sep).map{s=>s.toDouble}
        // best fitness column index : ?
        val best = values(4)
        val sol = values.drop(5).toVector
        solutions.append(HistoricalSolution("",1,1,best,sol))
      }
    }
    // select best solution and return
    solutions.sortWith((s1,s2)=>s1.bestFitnesses(0)(0) < s2.bestFitnesses(0)(0))(0)
  }


  def extractAndLoadData(f:File): Seq[HistoricalSolution] ={
    if(f.isFile) {
      if (f.toString.endsWith("gz")) {
        //uncompress the directories
        //println(f.toString)
        val dirname = f.toString.split("\\.")(0)
        val path = dirname.split("/")
        //println(dirname.split("/").take())
        ("rm -rf "+dirname).!
        ("tar -xf "+f.toString+" -C "+path.take(path.length - 1).mkString("/")).!
        extractAndLoadData(new File(dirname))
      }
      //load current csv
      if (f.toString.endsWith(".dat")) {
        Seq(dataload1dimension(f))
      }
      Seq.empty
    }

    if (f.isDirectory){
      f.listFiles().map{extractAndLoadData}.reduce((s1,s2)=>s1++s2)
    }
    Seq.empty
  }

  /**
    * Reads all historical result files and construct a synthesis file with optimal solutions
    * @param datadir
    * @param suiteName
    * @param targetSolutionFile
    */
  def synthesizeSolutions(datadir: String,suiteName:String,targetSolutionFile:String) = {
      val dir = new File(datadir)
      extractAndLoadData(dir)
  }


  def testResultExtraction() = {
    synthesizeSolutions("/home/raimbault/ComplexSystems/NoisyEA/Data/coco/bbob","","")
  }


}
