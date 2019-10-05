package file

import better.files._

object SaveFile {
  val dataDir = "./data/"

  def save(path: String)(data: String): Unit =
    File(dataDir + path) append data

  def saveCsv(path: String)(data: CSV): Unit = {
    val targetFile = File(dataDir + path)
    targetFile.clear()
    targetFile.append(data.headers.mkString(",") + "\n")
    data.data.foreach(csvLine => targetFile.append(csvLine.line + "\n"))
  }
}
