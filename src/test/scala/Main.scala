package xyz.hyperreal.yola

object Main extends App {

  import Testing._

  println(runCapture("""
                       |import yode.os._
                       |
                       |println( osUname() )
                       |""".stripMargin))

}
