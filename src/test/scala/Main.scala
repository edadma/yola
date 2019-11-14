package xyz.hyperreal.yola

object Main extends App {

  import Testing._

  println(runCapture("""
                       |val a :: (x@b :: c :: []) = [3, 4, 5]
                       |
                       |println( a, b, c, x )
                       |""".stripMargin))

}
