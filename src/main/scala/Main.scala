package xyz.hyperreal.yola

object Main extends App {

  val program =
    """
      |val a = 123
      |
      |println( a + 1 )
      |
      |def f(x) = x + 3
    """.stripMargin
  val parser = new YolaParser
  val ast    = parser.parseFromString(program, parser.source)

  println(ast)

}
