package file

import better.files.File
import finiteMonoids.CayleyTable

object ReadFile {
  val dataDir = "./data/"

  def read(path: String): String =
    File(dataDir + path).contentAsString

  def readCsv(path: String): CSV = {
    val targetFile = File(dataDir + path)
    val contents = targetFile.contentAsString.split("\n")
    val headers = contents.head.split(",").toList
    val data = contents.tail.toList.map(CSVLine)
    CSV(headers, data)
  }

  def lineToCayleyTable(line: String): CayleyTable[String] = CayleyTable {
    val elems = line.split(",").map(_.trim)
    val n = Math.sqrt(elems.length).toInt
    elems.grouped(n).toList.map(_.toList)
  }

  def readCSVToListOfCayleytables(path: String): List[CayleyTable[String]] = {
    readCsv(path).data.map(_.line).map(lineToCayleyTable)
  }

  readCSVToListOfCayleytables("monoids/order3.csv")
    .foreach(_.prettyPrint())
}
