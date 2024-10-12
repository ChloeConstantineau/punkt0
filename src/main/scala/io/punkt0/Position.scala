package io.punkt0

trait PositionT
case class Position(line: Int, column: Int) extends PositionT {
  def location: String = s"($line:$column)"
}
