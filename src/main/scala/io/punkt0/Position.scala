package io.punkt0

import java.io.File
import scala.io.Source

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
//  def show(file: File): String = {
//    val lines =
//      Source.fromFile(file).withPositioning(true).getLines().toIndexedSeq
//    s"""
//       |${lines(line - 1)}
//       |${" " * column + "^"}
//       |""".stripMargin
//  }
}
