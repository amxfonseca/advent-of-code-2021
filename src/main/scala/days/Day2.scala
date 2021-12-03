package days

import base.Day

enum Direction:
  case Forward, Up, Down

enum Command(val pos: Int, val depth: Int):
  case Forward(x: Int) extends Command(0, x)
  case Up(x: Int) extends Command(-x, 0)
  case Down(x: Int) extends Command(x, 0)

  case Move(x: Int, y: Int) extends Command(x, y)

  def +(cmd: Command): Command = Move(pos + cmd.pos, depth + cmd.depth)
  def product: Int = pos * depth

object Command {
  def fromString(s: String): Command = {
    val tokens = s.split(' ') match
      case Array(direction, amount) => (direction, amount.toIntOption)
      case _                        => ("_", None)

    tokens match
      case ("forward", Some(amount)) => Command.Forward(amount)
      case ("up", Some(amount))      => Command.Up(amount)
      case ("down", Some(amount))    => Command.Down(amount)
      case _                         => throw new NoSuchElementException

  }
}

object Day2 extends Day(2) {
  private val commands = input.getLines().map(Command.fromString).toList

  override def partOne(): String = commands.reduce(_ + _).product.toString
  override def partTwo(): String = ???
}
