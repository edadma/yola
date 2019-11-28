package xyz.hyperreal.yola

object Testing {

  def run(snippet: String, output: Any => Unit) = {
    val parser          = new YParser
    val ast             = parser.parseFromString(snippet, parser.source)
    implicit val global = globalScope

    global.vars("println") = (args: List[Any]) => output(args map display mkString ", ")

    new Interpreter(globalScope)(ast)
  }

  def runCapture(snippet: String) = {
    val out = new StringBuilder

    def output(a: Any): Unit = {
      out ++= a.toString
      out += '\n'
    }

    run(snippet, output)
    out.toString.trim
  }

  def runResult(snippet: String) = {
    val res = run(snippet, println)

    println(res)
    res
  }
}
