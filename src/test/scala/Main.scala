package xyz.hyperreal.yola

object Main extends App {

  import Testing._

  println(runCapture("""
                       |
                       |
                       |println( (x -> x + 3)(4) )
                       |""".stripMargin))

}
