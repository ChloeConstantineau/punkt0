package io.punkt0

case class Coordinates(y: Int, x: Int)

trait Positioned:
    def coordinates: Coordinates
    def line: Int        = coordinates.y
    def column: Int      = coordinates.x
    def location: String = s"($line:$column)"
