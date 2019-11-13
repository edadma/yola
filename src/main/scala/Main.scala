package xyz.hyperreal.yola

object Main extends App {

  val program =
    """
      |val (3|4, a) = (5, 4)
      |
      |println( a )
    """.stripMargin
  val parser            = new YParser
  val ast               = parser.parseFromString(program, parser.source)
  implicit val toplevel = new Scope(null)

  toplevel.vars("println") = (args: List[Any]) => println(args map display mkString ", ")
  println(Interpreter(ast))

}
