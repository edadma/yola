package xyz.hyperreal.yola

object Main extends App {

  import Testing._

  println(runCapture("""
                       |println( 'asdf' )
                       |""".stripMargin))

}
