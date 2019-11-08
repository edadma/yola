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

  toplevel.vars("println") = (args: List[Any]) => println(args)

  println(YolaInterpreter(ast))

}
