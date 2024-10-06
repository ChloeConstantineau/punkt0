package io.punkt0

object Position {
  private val LINE_BITS = 20
  private val COLUMN_BITS = 31 - LINE_BITS
  private val LINE_MASK = (1 << LINE_BITS) - 1
  private val COLUMN_MASK = (1 << COLUMN_BITS) - 1

  private def lineOf(pos: Int): Int =
    (pos >> COLUMN_BITS) & LINE_MASK

  private def columnOf(pos: Int): Int =
    pos & COLUMN_MASK
}

case class Position(line: Int, column: Int) {
  def location: String = s"($line:$column)"
}
