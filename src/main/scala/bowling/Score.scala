package bowling

import scala.annotation.tailrec

class Score(val firstRoll: Int, val secondRoll: Option[Int]){
  def closed: Boolean = !(secondRoll isEmpty)
  def score: Int = firstRoll + secondRoll.getOrElse(0)
  override def toString:String = s"($firstRoll, ${secondRoll.getOrElse(null)})"
}

case class Spare(override val firstRoll: Int, override val secondRoll: Option[Int]) extends Score(firstRoll, secondRoll) {
  override def closed: Boolean = true
}
case class Strike(override val firstRoll: Int) extends Score(firstRoll, None){
  override def closed: Boolean = true
}

object Score {

  def apply(firstRoll: Int): Score = {
    if( firstRoll == 10 ) new Strike(10)
    else new Score(firstRoll, None)
  }

  def apply(firstRollScore: Score, secondRoll: Int): Score = {
    if(firstRollScore.firstRoll + secondRoll == 10){
      new Spare(firstRollScore.firstRoll, Some(secondRoll))
    }
    else new Score(firstRollScore.firstRoll, Some(secondRoll))
  }
  def calculScore(scores: List[Score]): Int = {
    calculScoreRec(scores, 0, 0, 0)
  }

  @tailrec
  def calculScoreRec(scores: List[Score], cumul: Int, next: Int, nextsnext: Int): Int = {
    if( scores isEmpty ) cumul
    else if( scores.size == 12 ) calculScoreRec(
      scores.tail,
      cumul + scores.head.score,
      0,
      0,
    )
    else if( scores.size == 11 ) calculScoreRec(
      scores.tail,
      cumul + scores.head.score,
      scores.head.firstRoll,
      next,
    )
    else if( scores.size == 10 ) calculScoreRec(
      scores.tail,
      cumul + scores.head.score,
      scores.head.firstRoll,
      scores.head.secondRoll.getOrElse(next),
    )
    else {
      val frame = scores.head
      frame match {
        case Spare(firstRoll, Some(secondRoll)) => calculScoreRec(
          scores.tail,
          cumul + frame.score + next,
          firstRoll,
          secondRoll
        )
        case Strike(loneRoll) => calculScoreRec(
          scores.tail,
          cumul + frame.score + next + nextsnext,
          loneRoll,
          next
        )
        case _ => calculScoreRec(
          scores.tail,
          cumul + frame.score,
          frame.firstRoll,
          frame.secondRoll.getOrElse(next)
        )
      }
    }
  }
}