package days
import base.Day

object Day1 extends Day(1) {
  private val measures = input.getLines().map(_.toInt).toList

  extension (it: List[Int]) def countIncreasing = it.sliding(2).count { case Seq(fst, snd) => snd > fst }

  def partOne() = measures.countIncreasing.toString
  def partTwo() = {
    val averaged = measures.sliding(3).map(_.sum).toList
    averaged.countIncreasing.toString
  }

}
