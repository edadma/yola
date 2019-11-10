package xyz.hyperreal.yola

object Main extends App {

  val program =
    """
      |var a = 3
      |var b = 4
      |
      |def f(x) = x + 5
      |
      |while a-- > 0
      | println( a )
      |else
      | println( 'done' )
    """.stripMargin
  val parser            = new YolaParser
  val ast               = parser.parseFromString(program, parser.source)
  implicit val toplevel = new Scope
  val PRINT             = (args: List[Any]) => println(args mkString ", ")

  toplevel.vars("println") = PRINT
//  toplevel.vars("console") = Map("log" -> PRINT)
//  println(ast)
  YolaInterpreter(ast)

}
