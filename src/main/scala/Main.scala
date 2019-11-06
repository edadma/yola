package xyz.hyperreal.yola

object Main extends App {

  val program =
    """
      |var a = 3
      |
      |def f(x) = x + 4
      |
      |println( f(a) )
    """.stripMargin
  val parser = new YolaParser
  val ast    = parser.parseFromString(program, parser.source)
  val interp = new Interpreter

  println(interp(ast))

}
