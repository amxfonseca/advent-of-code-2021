package days

import base.Day

import scala.annotation.tailrec

type State = IndexedSeq[Long]

inline val YOUNG_FISH_TIMER = 6
inline val ADULT_FISH_TIMER = 8

object FishSchool {
  def initialState(s: String): State = {
    val initialValues = s
      .split(',')
      .map(_.toInt)
      .groupBy(identity)
      .view
      .mapValues(_.length.toLong)

    IndexedSeq.tabulate(ADULT_FISH_TIMER + 1) { initialValues.getOrElse(_, 0.toLong) }
  }

  @tailrec
  def populationAfter(initialState: State, days: Int): Long = days match {
    case 0 => initialState.sum
    case _ =>
      val hatching = initialState.head
      val shifted = initialState.tail :+ hatching
      val newState = shifted.updated(YOUNG_FISH_TIMER, shifted.apply(YOUNG_FISH_TIMER) + hatching)

      populationAfter(newState, days - 1)
  }
}

object Day6 extends Day(6) {
  val school: State = FishSchool.initialState(input.mkString)

  def partOne(): String = FishSchool.populationAfter(school, 80).toString
  def partTwo(): String = FishSchool.populationAfter(school, 256).toString
}
