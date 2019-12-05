package xyz.hyperreal.yola

object Testing {

  def run(snippet: String, output: Value => Value) = {
    val parser          = new YParser
    val ast             = parser.parseFromString(snippet, parser.source)
    implicit val global = new Scope(globalScope)

    global.values("println") = NativeFunction(
      (args: List[Value]) => output(args map (_.toString) mkString ", "))

    new Interpreter(globalScope)(ast)
  }

  def runCapture(snippet: String) = {
    val out = new StringBuilder

    def output(a: Value) = {
      out ++= a.toString
      out += '\n'
      YUnit
    }

    run(snippet, output)
    out.toString.trim
  }

  def runResult(snippet: String) = run(snippet, println)
}
