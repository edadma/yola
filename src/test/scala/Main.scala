package xyz.hyperreal.yola

object Main extends App {

  import Testing._

  println(runCapture("""
                       |data a(b, c)
                       |
                       |println( a(3, 4) )
                       |""".stripMargin))

}
