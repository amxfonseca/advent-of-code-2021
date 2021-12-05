package days

import base.Day

import scala.annotation.tailrec
import scala.collection.immutable.VectorMap

type GameState = VectorMap[Int, Boolean]
enum BingoGame {
  case Ongoing(size: Int, g: GameState)
  case Finished(lastCall: Int, g: GameState)
}

object BingoGame {
  def create(data: Iterable[Int]): BingoGame = {
    val size = math.sqrt(data.size) match
      case x if x.isValidInt => x.intValue
      case _                 => throw new IllegalArgumentException

    val gameState = VectorMap.from(data.map((_, false)))
    BingoGame.Ongoing(size, gameState)
  }

  def findWinningSequence(state: GameState, idx: Int, size: Int): Option[Vector[Int]] = {

    val rowStartIndex = idx - (idx % size)
    val row = state.slice(rowStartIndex, rowStartIndex + size)
    val col = state.drop(idx % size).sliding(1, size).flatten.toVector

    (row, col) match
      case (row, _) if row.forall(_._2) => Some(row.keys)
      case (_, col) if col.forall(_._2) => Some(col.map(_._1))
      case _                            => None

  }

  def nextState(g: BingoGame, number: Int) = g match
    case Finished(_, _)                               => g
    case Ongoing(_, state) if !state.contains(number) => g
    case Ongoing(size, state) =>
      val newState = state.updated(number, true)
      val index = newState.keys.indexOf(number)

      findWinningSequence(newState, index, size) match
        case Some(_) => Finished(number, newState)
        case None    => Ongoing(size, newState)

  def parseInputs(data: Iterable[String]) = data.flatMap(_.split(',')).map(_.toInt)
  def parseGames(data: Iterable[String]) =
    val cleanData = data.dropWhile(_ == "")
    val gameSize = cleanData.headOption match
      case Some(str) => str.split(' ').size
      case _         => throw new IllegalArgumentException

    val parseLine: String => Iterable[Int] = _.split(' ').flatMap(_.toIntOption)
    cleanData.filterNot(_ == "").grouped(gameSize).map(_.flatMap(parseLine))

}

object Day4 extends Day(4) {
  val (rawInputs, rawGames) = input.getLines().toSeq.splitAt(1)
  val inputs = BingoGame.parseInputs(rawInputs)
  val games = BingoGame.parseGames(rawGames).map(BingoGame.create).toSeq

  def partOne() = {

    @tailrec
    def findWinningGame(games: Iterable[BingoGame], inputs: Iterable[Int]): Option[BingoGame.Finished] =
      inputs match {
        case Nil => None
        case x :: xs =>
          val gamesAfterMove = games.map(BingoGame.nextState(_, x))
          gamesAfterMove.find(_.isInstanceOf[BingoGame.Finished]) match {
            case Some(game @ BingoGame.Finished(_, _)) => Some(game)
            case _                                     => findWinningGame(gamesAfterMove, xs)
          }
      }

    val response = findWinningGame(games, inputs) match {
      case Some(BingoGame.Finished(lastMove, state)) => lastMove * state.filterNot(_._2).keys.sum
      case _                                         => 0
    }

    response.toString

  }
  def partTwo() = ""
}
