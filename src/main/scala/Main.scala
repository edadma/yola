package xyz.hyperreal.yola

object Main extends App {

  val program =
    """
      |var a = 3
      |var b = 4
      |
      |def f(x) = x + b
      |
      |val [c, d] = [5, 6]
      |
      |println( "asdf ${[d, c]} wert $a." )
    """.stripMargin
  val parser            = new YolaParser
  val ast               = parser.parseFromString(program, parser.source)
  implicit val toplevel = new Scope(null)

  toplevel.vars("println") = (args: List[Any]) => println(args map display mkString ", ")
  YolaInterpreter(ast)

}
