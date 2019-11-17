package xyz.hyperreal.yola

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

object Testing {

  def runCapture(snippet: String) = {
    val parser            = new YParser
    val ast               = parser.parseFromString(snippet, parser.source)
    implicit val toplevel = new Scope(null)
    val out               = new StringBuilder

    def output(a: Any) = {
      out ++= a.toString
      out += '\n'
    }

    toplevel.vars("println") = (args: List[Any]) => output(args map display mkString ", ")
    new Interpreter(null)(ast)
    out.toString.trim
  }

  def runResult(snippet: String) = {
    val parser            = new YParser
    val ast               = parser.parseFromString(snippet, parser.source)
    implicit val toplevel = new Scope(null)

    new Interpreter(null)(ast)
  }

}
