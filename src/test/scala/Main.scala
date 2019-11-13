package xyz.hyperreal.yola

object Main extends App {

  import Testing._

  println(runCapture("""
                       |val a :: b = [3, 4]
                       |
                       |println( a, b )
                       |""".stripMargin))

}
