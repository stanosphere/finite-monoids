package file

case class CSVLine(line: String)

case class CSV(headers: List[String], data: List[CSVLine])
