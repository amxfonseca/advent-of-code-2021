package days

import base.Day
import scala.annotation.tailrec

type Sample = Vector[Int]

object Sample {
  def fromString(s: String): Sample = Vector.from(s.map(c => if c == '0' then -1 else 1))
  def getGamma(sample: Sample): Int = sample.toInt
  def getEpsilon(sample: Sample): Int = ~Sample.getGamma(sample) & ((1 << sample.length) - 1)
}

extension (sample: Sample)
  def +(other: Sample): Sample = sample.zip(other).map { case (a, b) => a + b }
  def toInt: Int = sample.map(c => if c > 0 then 1 else 0).reverse.zipWithIndex.foldLeft(0) { case (output, (b, i)) =>
    output | (b << i)
  }

object Day3 extends Day(3) {
  private val samples = input.getLines().map(Sample.fromString).toVector

  def partOne(): String = {
    val accumulated = samples.reduce(_ + _)
    (Sample.getGamma(accumulated) * Sample.getEpsilon(accumulated)).toString
  }

  def partTwo(): String = {

    @tailrec
    def getGasRating(samples: Vector[Sample], cmp: (Int, Int) => Boolean, idx: Int = 0): Sample =
      samples match
        case Vector(head) => head
        case Vector()     => Vector()
        case _ =>
          val (highSamples, lowSamples) = samples.partition(_(idx) == 1)
          val remainingSamples = if cmp(highSamples.size, lowSamples.size) then highSamples else lowSamples

          getGasRating(remainingSamples, cmp, idx + 1)

    val o2Rating = getGasRating(samples, _ >= _).toInt
    val co2Rating = getGasRating(samples, _ < _).toInt

    (o2Rating * co2Rating).toString
  }

}
