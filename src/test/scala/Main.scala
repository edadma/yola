package xyz.hyperreal.yola

object Main extends App {
  Testing.runResult(io.Source.fromFile(args(0)) mkString)
}
