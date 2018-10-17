package bowling

// TODO : Avoid using .get

case class Frame(number: Int, firstRoll: Option[Int], secondRoll: Option[Int] = None, lastRoll: Option[Int] = None) {
  def isDone: Boolean = number match {
    case 10 => firstRoll.nonEmpty && secondRoll.nonEmpty && secondRoll.nonEmpty
    case _ => firstRoll.nonEmpty && secondRoll.nonEmpty
  }

  def isNotDone: Boolean = !isDone
  def isLast: Boolean = number == 10

  def isSpare: Boolean = (firstRoll.get + secondRoll.get == 10) && !isStrike

  def isStrike: Boolean = firstRoll.get == 10
}

case class Game(score: Int = 0, frames: List[Frame] = List(), bonusRolls: Int = 0) {
  def roll(pin: Int): Game = {
    var newScore = score
    var newFrames = frames
    var newBonusRolls = bonusRolls

    if (frames.nonEmpty) {

      val lastFrame = frames.head

      if (lastFrame.isNotDone) {
        newFrames = Frame(lastFrame.number, lastFrame.firstRoll, Some(pin)) :: frames.tail
        if (newFrames.head.isSpare) {
          newBonusRolls += 1
        }
      }
      if (lastFrame.isLast) {// last roll
        newFrames = Frame(lastFrame.number, lastFrame.firstRoll, lastFrame.secondRoll, Some(pin)) :: frames.tail
      }
      else {// new frame
        if (pin == 10) {
          newFrames = Frame(lastFrame.number + 1, Some(pin), Some(0)) :: frames
          newBonusRolls += 2
        }
        else {
          newFrames = Frame(lastFrame.number + 1, Some(pin)) :: frames
        }
      }
    }
    else { // first roll
      if (pin == 10){
        newFrames = List(Frame(1, Some(pin), Some(0)))
        newBonusRolls += 2
      }
      else {
        newFrames = List(Frame(1, Some(pin)))
      }
    }

    newScore += pin

    if (bonusRolls > 2) {// if more than two bonuses are pending
      newScore += pin * 2
      newBonusRolls -= 2
    }
    else if (bonusRolls > 0) {
      newScore += pin
      newBonusRolls -= 1
    }

    copy(newScore, newFrames, newBonusRolls)
  }
}

object Bowling extends App {
  val game = Game()
  println(game.roll(10).roll(10).score)
  println(game.roll(10).roll(10).roll(10).score)
  println(game.roll(10).roll(10).roll(10).roll(10).score)
  println(game.roll(10).roll(10).roll(10).roll(10).roll(10).roll(10).roll(10).roll(10).roll(10).roll(10).roll(10).score)
}
