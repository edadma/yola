package xyz.hyperreal.yola

object Testing {

  def run(snippet: String, output: Any => Unit) = {
    val parser          = new YParser
    val ast             = parser.parseFromString(snippet, parser.source)
    implicit val global = new Scope(globalScope)

    global.values("println") = NativeFunction(
      (args: List[Value]) => {
        output(args map (_.toString) mkString ", ")
        YUnit
      }
    )

    new Interpreter(globalScope)(ast)
  }

  def runCapture(snippet: String) = {
    val out = new StringBuilder

    def output(a: Any) = {
      out ++= a.toString
      out += '\n'
    }

    run(snippet, output)
    out.toString.trim
  }

  def runResult(snippet: String) = run(snippet, println)
}
