package xyz.hyperreal.yola

object Testing {

  def loader(module: List[String], name: String, rename: Option[String], scope: Scope): Unit = {}

  def runCapture(snippet: String) = {
    val parser          = new YParser
    val ast             = parser.parseFromString(snippet, parser.source)
    implicit val global = new Scope(null)
    val out             = new StringBuilder

    def output(a: Any): Unit = {
      out ++= a.toString
      out += '\n'
    }

    global.vars("println") = (args: List[Any]) => output(args map display mkString ", ")
    new Interpreter(loader)(ast)
    out.toString.trim
  }

  def runResult(snippet: String) = {
    val parser          = new YParser
    val ast             = parser.parseFromString(snippet, parser.source)
    implicit val global = new Scope(null)

    new Interpreter(loader)(ast)
  }

}
