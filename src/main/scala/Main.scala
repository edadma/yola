package xyz.hyperreal.yola

object Main extends App {

  val program =
    """
      |var a = 3
      |
      |def f(x) = x + 4
      |
      |console.log( f(a) )
    """.stripMargin
  val parser            = new YolaParser
  val ast               = parser.parseFromString(program, parser.source)
  implicit val toplevel = new Scope
  val PRINT             = (args: List[Any]) => println(args mkString ", ")

  toplevel.vars("println") = PRINT
  toplevel.vars("console") = Map("log" -> PRINT)
  println(ast)
  YolaInterpreter(ast)

}
