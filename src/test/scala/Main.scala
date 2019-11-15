package xyz.hyperreal.yola

object Main extends App {

  import Testing._

  println(runCapture("""
                       |for i <- 1..4 if i%2 == 0; j <- 5..8 if j%2 == 1
                       |  println( i, j )
                       |""".stripMargin))

}
