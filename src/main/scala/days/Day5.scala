package days

import base.Day

type Point = (Int, Int)
type Line = (Point, Point)

object Line {
  def parsePoint(p: String): Point = p.split(',').map(_.toInt) match { case Array(x, y) => (x, y) }
  def fromString(s: String): Line = {
    val Pattern = raw"(\d+,\d+) -> (\d+,\d+)".r

    val (start, end) = s match
      case Pattern(start, end) => (start, end)
      case _                   => throw new IllegalArgumentException

    (parsePoint(start), parsePoint(end))

  }
  def isStraight(line: Line): Boolean = {
    val ((x1, y1), (x2, y2)) = line
    x1 == x2 || y1 == y2
  }

  def range(start: Int, end: Int): Range = {
    val step = if start < end then 1 else -1
    start to end by step
  }

  def expand(line: Line): Seq[Point] = {
    val ((x1, y1), (x2, y2)) = line
    range(x1, x2).zipAll(range(y1, y2), x2, y2)
  }
}

case class Plotter(state: Map[Point, Int] = Map.empty) {
  def :+(points: Seq[Point]): Plotter = {
    val newState = points.foldLeft(state)(_.updatedWith(_) {
      case Some(value) => Some(value + 1)
      case _           => Some(1)
    })

    Plotter(newState)
  }

  def intersections: Int = state.filter(_._2 > 1).size
}

object Day5 extends Day(5) {
  val lines = input.getLines().map(Line.fromString).toSeq

  def partOne(): String = {
    val expandedStraightLines = lines.filter(Line.isStraight).map(Line.expand)
    val intersections = expandedStraightLines.foldLeft(Plotter())(_ :+ _).intersections
    intersections.toString
  }

  def partTwo(): String = {
    val expandedLines = lines.map(Line.expand)
    val intersections = expandedLines.foldLeft(Plotter())(_ :+ _).intersections
    intersections.toString
  }
}
