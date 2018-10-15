
package mgobench.problem.coco

import java.io.File

import scala.annotation.tailrec
//import scala.collection.mutable.ArrayBuffer
import scala.collection._
import scala.io._
import scala.sys.process._

import mgobench.utils.io.File._

object CocoSolutions {

  /**
    * Loading for 1d -> similar for 2objs ? (bbob-biobj)
    * @param f
    * @return
    */
  def dataload1dimension(f:File,sep: String = " ",suiteName: String = "bbob"): Seq[HistoricalSolution] = {
    println("Loading data file "+f.toString)
    // function name and dimensions contained in file name
    val ndimstr = f.getName.replace(".dat","").replace(suiteName+"_","").replace("DIM","")
    val fun = ndimstr.split("_")(1)
    val dim = ndimstr.split("_")(2).toInt
    var inst = "1";if(ndimstr.split("_").size>3){inst = ndimstr.split("_")(3).trim}
    println("Function "+fun+" ; dim "+dim)
    val solutions = mutable.ArrayBuffer[HistoricalSolution]()
    // get function name and dimension from file name
    //val instances = (1 to 5) ++ (71 to 80)
    // instances must be loaded from info file in the directory above
    if(new File(f.getParentFile.getParent+"/bbobexp_"+fun+".info").exists||new File(f.getParentFile.getParent+"/bbobexp_"+fun+"_i"+inst+".info").exists) {
      val path = if(new File(f.getParentFile.getParent+"/bbobexp_"+fun+".info").exists) f.getParentFile.getParent+"/bbobexp_"+fun+".info" else f.getParentFile.getParent+"/bbobexp_"+fun+"_i"+inst+".info"
      val infos = Source.fromFile(path).getLines().toList.filter(_.contains(f.getName))

      if (infos.size > 0) {
        val instances = infos(0).split(",").tail.map {
          _.split(":")(0).trim.toInt
        }

        var currentInstanceIndex = -1
        var currentbest = 100000.0
        var currentsol = Vector.fill(dim)(0.0)
        var thbest = 0.0
        val lines = Source.fromFile(f).getLines().toList

        assert(lines.map { case s => if (s.startsWith("%")) 1.0 else 0.0 }.sum == instances.size, s"Issue with file ${f.getAbsolutePath}")
        //if (lines.map{case s => if (s.startsWith("%"))1.0 else 0.0}.sum == 15) {
        for (l <- lines) {
          if (!(l.startsWith("%") || l.contains("+inf"))) {
            // comments and infinite values
            val values: Array[Double] = l.split(sep).map {
              case s => s.toDouble //assert(s.length>0,s"error : $l");s.toDouble
            }
            // best fitness column index : ?
            val best = values(4)
            //val sol = values.drop(6).toVector // left to drop not robust : issue space indent ?
            val sol = values.takeRight(dim).toVector
            if (best < currentbest) {
              currentbest = best;
              currentsol = sol
            }
          }
          if (l.startsWith("%")) {
            // header line or last line
            //println(l.split("\\|").filter(_.contains("noise-free fitness - Fopt ("))(0).split("\\(")(1).replace(" ","").replace(")",""))
            if (currentInstanceIndex >= 0) {
              solutions.append(HistoricalSolution(fun, dim, instances(currentInstanceIndex), thbest, currentsol))
              currentbest = 100000.0
              currentsol = Vector.fill(dim)(0.0)
            }

            thbest = l.split("\\|").filter(_.contains("noise-free fitness - Fopt ("))(0).split("\\(")(1).replace(" ", "").replace(")", "").toDouble
            currentInstanceIndex = currentInstanceIndex + 1

          }
        }
        solutions.append(HistoricalSolution(fun, dim, instances(currentInstanceIndex), thbest, currentsol))

        //val sols = solutions.size
        //val insts = solutions.map{_.instance}
        //assert(solutions.size==15,s"Missing instances for fun $fun dim $dim ; instances : $sols ; ${insts}")
        // select best solution and return
        //solutions.sortWith((s1,s2)=>s1.bestFitnesses(0)(0) < s2.bestFitnesses(0)(0))(0)
        //if (solutions.size == 15) solutions else Seq.empty
        //}else Seq.empty
        solutions
      } else Seq.empty
    }else Seq.empty
  }


  def extractAndLoadData(f:File,suiteName:String): Seq[HistoricalSolution] ={
    println("Loading "+f.getAbsolutePath)
    if(f.isFile) {
      if (f.toString.endsWith("gz")) {
        //uncompress the directories
        //println(f.toString)
        val dirname = f.toString.split("\\.")(0)
        val path = dirname.split("/")
        //println(dirname.split("/").take())
        ("rm -rf "+dirname).!
        ("tar -xf "+f.toString+" -C "+path.take(path.length - 1).mkString("/")).!
        extractAndLoadData(new File(dirname),suiteName)
      }
      //load current csv
      if (f.toString.endsWith(".dat")) {
        return(dataload1dimension(f))
      }
      return(Seq.empty)
    }

    if (f.isDirectory){
      if (f.listFiles.size>0) {
        return(f.listFiles().map {
          extractAndLoadData(_,suiteName)
        }.reduce((s1, s2) => s1 ++ s2)
          )
      }else{return(Seq.empty)}
    }else{return(Seq.empty)}

  }

  /**
    * Reads all historical result files and construct a synthesis file with optimal solutions
    * @param datadir
    * @param suiteName
    * @param targetSolutionFile
    * @param aggregationFunction in theory should be able to retrieve Pareto fronts - hist results are only in 1D so no need ; use an aggreg function
    */
  def synthesizeSolutions(datadir: String,suiteName:String,targetSolutionFile:String,aggregationFunction: Vector[Double]=> Double = _.sum): Unit = {
    val dir = new File(datadir)
    val historicalResults = extractAndLoadData(dir,suiteName)

    //println(historicalResults)

    // group by function and dimension to get the best result
    val bests = new mutable.HashMap[(String,Int),HistoricalSolution]()
    historicalResults.foreach{case s =>
      if (bests.contains((s.cocofunction+"_"+s.instance,s.dimension))){
        val currentsol = bests((s.cocofunction+"_"+s.instance,s.dimension))
        if (currentsol.bestFitnesses.map{aggregationFunction}.min > s.bestFitnesses.map{aggregationFunction}.min){
          bests((s.cocofunction+"_"+s.instance,s.dimension)) = s
        }
      }else{
        bests((s.cocofunction+"_"+s.instance,s.dimension))=s
      }
    }

    //println(bests.toMap)

    // write to file
    // no pareto front for now : take the first and only
    writeCSV(bests.toMap.map{case ((f,d),h) => Array(f,d.toString,h.bestFitnesses(0).mkString(","),h.bestSolutions(0).mkString(",")) }.toArray,targetSolutionFile,";")
  }


  /**
    * Extract all results from historical files
    */
  def runResultExtraction(): Unit = {
    synthesizeSolutions(sys.env.get("CS_HOME").get+"/NoisyEA/Data/coco/bbob",suiteName = "bbob",targetSolutionFile = "data/historicalresults.csv")
    //synthesizeSolutions(sys.env.get("CS_HOME").get+"/NoisyEA/Data/coco/bbob/2009/ALPS",suiteName = "bbob",targetSolutionFile = "data/historicalresults.csv")
  }

  /**
    * Get historical files from synthesis file
    * @param file
    * @return map (function,dimension) -> HistoricalSolution - used for computation of expected indicators
    */
  def loadSolutions(file: String): Map[(String,Int),HistoricalSolution] = {
    val raw  = readCSV(file,";")
    raw.map{case r =>
      ((r(0),r(1).toInt),HistoricalSolution(r(0),r(1).toInt,1,r(2).toDouble,r(3).split(",").map{_.toDouble}.toVector))
    }.toMap
  }

  /**
    * Test
    */
  def testResultExtraction(): Unit = {
    println("Test result file loading on dir "+sys.env.get("CS_HOME")+"/NoisyEA/Data/coco/bbob")
    synthesizeSolutions(sys.env.get("CS_HOME").get+"/NoisyEA/Data/coco/test",suiteName = "bbob",targetSolutionFile = "data/testhistresults.csv")
  }


}
