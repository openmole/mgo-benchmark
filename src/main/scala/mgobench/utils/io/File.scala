
package mgobench.utils.io

import java.io._
import scala.io._

object File {

  /**
    *
    * @param d
    * @param file
    * @param delimiter
    */
  def writeCSV(d: Array[Array[String]],file: String,delimiter: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(file)))
    d.foreach{case r => writer.write(r.mkString(delimiter));writer.newLine()}
    writer.close()
  }

  def readCSV(f: String, delimiter: String): Array[Array[String]] = {
    Source.fromFile(f).getLines().map{_.split(delimiter)}.toArray

    /*val reader = new BufferedReader(new FileReader(f))
    var currentLine = reader.readLine()
    var res = List((currentLine.split(delimiter).map { s => s.toDouble })) //= new List[Array[Double]] {}
    while (currentLine != null) {
      currentLine = reader.readLine()
      if (currentLine != null) res = res :+ (currentLine.split(delimiter).map { s => s.toDouble }) //+: res
    }
    res.toArray*/
  }


}
