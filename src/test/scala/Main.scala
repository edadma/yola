package xyz.hyperreal.yola

object Main extends App {
  val src  = io.Source.fromFile(args(0))
  val code = src mkString

  src.close
  Testing.runResult(code)
}
