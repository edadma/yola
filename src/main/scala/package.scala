package xyz.hyperreal

import scala.util.parsing.input.Position

package object yola {

  def problem( pos: Position, error: String ) =
    if (pos eq null)
      sys.error( error )
    else
      sys.error( s"${pos.line}: $error\n${pos.longString}" )

}