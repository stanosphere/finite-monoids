package helpers

object Timed {
  def apply[R](name: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    val secs = (t1 - t0).toDouble / 1000
    println(s"Timed $name: $secs sec")
    result
  }
}
