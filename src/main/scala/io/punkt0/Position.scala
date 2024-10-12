package io.punkt0

case class Position(line: Int, column: Int) {
  def location: String = s"($line:$column)"
}
