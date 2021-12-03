package base

import scala.io.{BufferedSource, Source}

trait Day(val day: Int) {
  protected lazy val input: BufferedSource = Source.fromResource(s"input-${day}")
  def partOne(): String
  def partTwo(): String
}
