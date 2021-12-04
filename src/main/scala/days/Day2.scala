package days

import base.Day

enum Command:
  case Forward(n: Int)
  case Up(n: Int)
  case Down(n: Int)

object Command {
  def fromString(s: String): Command = {
    val Pattern = raw"(\w+)\s(\d+)".r
    val tokens = s match
      case Pattern(direction, amount) => (direction, amount.toIntOption)
      case _                          => ("", None)

    tokens match
      case ("forward", Some(amount)) => Command.Forward(amount)
      case ("up", Some(amount))      => Command.Up(amount)
      case ("down", Some(amount))    => Command.Down(amount)
      case _                         => throw new NoSuchElementException

  }
}

trait Movable {
  val horizontal: Int
  val depth: Int
  def move(cmd: Command): Movable
  def location: Int = horizontal * depth
}

case class Position(horizontal: Int = 0, depth: Int = 0) extends Movable {
  def move(cmd: Command): Position =
    cmd match
      case Command.Forward(n) => Position(horizontal + n, depth)
      case Command.Up(n)      => Position(horizontal, depth - n)
      case Command.Down(n)    => Position(horizontal, depth + n)
}

case class EnhancedPosition(horizontal: Int = 0, depth: Int = 0, aim: Int = 0) extends Movable {
  def move(cmd: Command): EnhancedPosition = {
    cmd match
      case Command.Forward(n) => EnhancedPosition(horizontal + n, depth + aim * n, aim)
      case Command.Up(n)      => EnhancedPosition(horizontal, depth, aim - n)
      case Command.Down(n)    => EnhancedPosition(horizontal, depth, aim + n)
  }
}

object Day2 extends Day(2) {
  private val commands = input.getLines().map(Command.fromString).toList

  override def partOne(): String = commands.foldLeft(Position())(_.move(_)).location.toString
  override def partTwo(): String = commands.foldLeft(EnhancedPosition())(_.move(_)).location.toString
}
