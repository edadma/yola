package xyz.hyperreal

import scala.util.parsing.input.Position

package object yola {

  def problem(pos: Position, error: String) = {
    if (pos eq null)
      println(error)
    else
      println(s"${pos.line}: $error\n${pos.longString}")

    sys.exit(1)
  }

}
