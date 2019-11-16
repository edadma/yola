package xyz.hyperreal.yola

object Main extends App {

  import Testing._

  println(runCapture("""
                       |val l =
                       |  for
                       |    i <- 1..2
                       |    j <- 3..4
                       |    yield
                       |      (i, j)
                       |
                       |println( l )
                       |""".stripMargin))

}
