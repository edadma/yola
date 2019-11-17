package xyz.hyperreal.yola

object Main extends App {

  import Testing._

  println(runCapture("""
                       |data a(b, c)
                       |
                       |val a(d, e) = a(3, 4)
                       |
                       |println( d, e )
                       |""".stripMargin))

}
