package bowling

import scala.annotation.tailrec
import org.scalatest.{FlatSpec, Matchers}

// Game
// Frame
// Pins
// score
// Strikes
// Spares
// Roll

class BowlingSpec extends FlatSpec with Matchers {

  class TestGame(game: Game = Game()) extends Game {
    override val score = game.score
    override val end = game.end
    @tailrec
    final def allRoll(pinDown: Int, gameRec: Game = game): TestGame = {
      if( gameRec end ) new TestGame(gameRec)
      else allRoll(pinDown, gameRec roll(pinDown))
    }

    @tailrec
    final def lastFrame(gameRec: Game = game): Game = {
      if( gameRec.leftFrames == 1) gameRec
      else lastFrame(gameRec.roll(0))
    }
  }

  "Two roll 2 in a new game" should "close a Frame with a score of 4" in {
    val aNewGame = new TestGame()
    val startingFrames = aNewGame leftFrames
    val twiceRolledGame = aNewGame roll 2 roll 2
    twiceRolledGame.leftFrames shouldBe startingFrames-1
    twiceRolledGame.score shouldBe 4
  }

  "Strike before last frame" should "close the frame" in {
    val aNewGame = new TestGame()
    val startingFrames = aNewGame leftFrames
    val StrikeRolledGame = aNewGame roll 10
    StrikeRolledGame.leftFrames shouldBe startingFrames-1
  }

  "Strikes on last frame" should "add an additionnal rolls" in {
    val anAlmostGameOver = new TestGame().lastFrame()
    val aStillNotGameOver = anAlmostGameOver roll 10 roll 10
    println(aStillNotGameOver.roll(10).roll(10))
    aStillNotGameOver.additionnalRoll shouldBe true
  }

  "Spare on last frame" should "add an additionnal rolls" in {
    val anAlmostGameOver = new TestGame().lastFrame()
    val aStillNotGameOver = anAlmostGameOver roll 5 roll 5
    aStillNotGameOver.additionnalRoll shouldBe true
  }

  "All roll 0 pin down" should "give a score of 0" in {
    val aNewGame = new TestGame()
    val allRollGame = aNewGame allRoll 0
    allRollGame.score shouldBe 0
  }

  "All strike" should "give a score of 300" in {
    val aNewGame = new TestGame()
    val allRollGame = aNewGame allRoll 10
    allRollGame.score shouldBe 300
  }

  "All roll 5 pin down (spare)" should "give a score of 150" in {
    val aNewGame = new TestGame()
    val allRollGame = aNewGame allRoll 5
    allRollGame.score shouldBe 150
  }

  "All roll 1 pin down" should "give a score of 20" in {
    val aNewGame = new TestGame()
    val allRollGame = aNewGame allRoll 1
    allRollGame.score shouldBe 20
  }
}
