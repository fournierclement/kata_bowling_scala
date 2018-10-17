package bowling

case class Game(previousFrames: List[Score] = Nil ){
  final val totalFrame = 10;

  def additionnalRoll: Boolean = {
    previousFrames
    .dropRight(totalFrame-1)
    .map({
      case Spare(first, second) => -2
      case Strike(lone) => -1
      case score => if( score.closed ) -3 else -1
    })
    .fold(3)(_+_) > 0
  }

  def leftFrames = {
    if(previousFrames.isEmpty) totalFrame
    else if (previousFrames.head.closed) totalFrame - previousFrames.size
    else totalFrame - (previousFrames.size - 1)
  }

  def end = !(leftFrames > 0 || additionnalRoll)

  def score = Score.calculScore(previousFrames)

  def roll(pinDown: Int): Game = {
    if( end ) this else {
      if( previousFrames.isEmpty || previousFrames.head.closed ) Game( Score(pinDown) :: previousFrames )
      else Game( Score(previousFrames.head, pinDown) :: previousFrames.tail )
    }
  }

}