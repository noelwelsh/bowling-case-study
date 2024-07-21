package code

import org.scalatest.wordspec.AnyWordSpecLike

class SpecificGamesSpec extends AnyWordSpecLike {
  "The game" should {
    // This test was in the spec
    "score ten open frames (each one a 9 followed by a miss)" in {
      val game = Game(
        frames = List.fill(9)(Frame.open(9, 0)),
        finalFrame = FinalFrame.open(9, 0)
      )

      assert(game.score == 90)
    }

    "score the nearly perfect game" in {
      val game = Game(
        frames = List.fill(9)(Frame.strike),
        finalFrame = FinalFrame.spare(2, 6)
      )

      assert(game.score == 268)
    }
  }
}