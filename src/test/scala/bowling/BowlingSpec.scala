package bowling

import org.scalatest.matchers._
import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

case class Gen() extends Random {
  def chooseNum(min : Int, max: Int): Int = nextInt(max) + min
}

object BowlingOperations {
  def gutter: GameInstruction = game => game.roll(0)
  def strike: GameInstruction = game => game.roll(10)

  implicit class PinDown(val pinDownValue: Int) extends AnyVal {
    def pinDown: GameInstruction = game => game.roll(pinDownValue)
    def pinDownThenSpare: GameInstruction = this.pinDown.andThen((10 - pinDownValue).pinDown)
  }

  def rollingInto(instruction: GameInstruction): Object {
    def times(time: Int): GameInstruction
  } = new {
    def times(time: Int): GameInstruction = game => (1 to time).foldLeft(game)((game, _) => instruction(game))
  }

  def aGameWith(instruction: GameInstruction): Game = instruction(Game)

  val pinDownValue: Int = Gen().chooseNum(1,9)

  def scoreOf(expectedScore: Int): HavePropertyMatcher[Game, Int] =
    (game: Game) => HavePropertyMatchResult(
      game.score == expectedScore,
      "score",
      expectedScore,
      game.score
    )
}

class BowlingSpec extends FunSpec with Matchers {

  import BowlingOperations._

  describe("Bowling"){
    it("should be 0 when all roll into guter") {
      aGameWith(
        rollingInto(gutter).times(20)
      ) should have(scoreOf(0))
    }

    it("should be 20 when all roll make 1 pin down"){
      aGameWith(
        rollingInto(1 pinDown).times(20)
      ) should have(scoreOf(20))
    }

    it("should count 10 for a spare when there is a gutter right after"){
      aGameWith{
        1 pinDownThenSpare
      } should have(scoreOf(10))
    }
  }
}
