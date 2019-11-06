package xyz.hyperreal.yola

object Main extends App {

  val program =
    """
      |var a = 123
      |
      |println( a + 1 )
    """.stripMargin
  val parser = new YolaParser
  val ast    = parser.parseFromString(program, parser.source)
  val interp = new Interpreter

  println(interp(ast))

}
