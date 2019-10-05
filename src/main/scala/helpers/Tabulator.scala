package helpers

import finiteMonoids.CayleyTable

object Tabulator {
  def separatedWithSpace(x: String, y: String): String = s"$x  $y"

  def separatedByBars(x: String, y: String): String =
    s"| $x | $y |"

  def tabulateCayley(symbolicTable: CayleyTable[String]): String = {
    val topRow = separatedByBars(" ", symbolicTable.table.head.reduce(separatedWithSpace))
    val separatorRow = "+" + "-" * 3 + "+" + "-" * (topRow.length - 6) + "+"

    val lines = List(separatorRow, topRow, separatorRow) ++
      symbolicTable.table.map(row =>
        separatedByBars(row.head, row.reduce(separatedWithSpace))
      ) :+ separatorRow
    lines.reduce(_ + "\n" + _)
  }
}
