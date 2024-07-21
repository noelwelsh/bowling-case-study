package code

final case class Game(frames: List[Frame], finalFrame: FinalFrame) {
  import Game._

  def score: Int = {
    val state =
      frames
        .foldLeft(initialState) { (state: State, frame: Frame) =>
          state.pending match {
            case Some(value) =>
              value match {
                case Strike =>
                  frame match {
                    case Frame.Strike =>
                      state.next(0, Some(StrikeAndStrike))

                    case Frame.Spare(roll) =>
                      state.next(10 + 10, Some(Spare))

                    case Frame.Open(roll1, roll2) =>
                      state.next(roll1 + roll2 + (10 + roll1 + roll2), None)
                  }

                case StrikeAndStrike =>
                  frame match {
                    case Frame.Strike =>
                      state.next(30, Some(StrikeAndStrike))

                    case Frame.Spare(roll) =>
                      state.next(10 + 10 + roll + (10 + 10), Some(Spare))

                    case Frame.Open(roll1, roll2) =>
                      state.next(10 + 10 + roll1 + (10 + roll1 + roll2), None)
                  }

                case Spare =>
                  frame match {
                    case Frame.Strike =>
                      state.next(10 + 10, Some(Strike))

                    case Frame.Spare(roll) =>
                      state.next(10 + roll, Some(Spare))

                    case Frame.Open(roll1, roll2) =>
                      state.next(10 + roll1 + (roll1 + roll2), None)
                  }
              }
            case None =>
              frame match {
                case Frame.Strike =>
                  state.next(0, Some(Strike))

                case Frame.Spare(roll) =>
                  state.next(0, Some(Spare))

                case Frame.Open(roll1, roll2) =>
                  state.next(roll1 + roll2, None)
              }
          }
        }

    state.pending match {
      case Some(value) =>
        value match {
          case Strike =>
            finalFrame match {
              case FinalFrame.Strike(bonus1, bonus2) =>
                state.score + (10 + 10 + bonus1) + (10 + bonus1 + bonus2)
              case FinalFrame.Spare(roll, bonus) =>
                state.score + (10 + 10) + (10 + bonus)
              case FinalFrame.Open(roll1, roll2) =>
                state.score + (10 + roll1 + roll2) + (roll1 + roll2)
            }
          case StrikeAndStrike =>
            finalFrame match {
              case FinalFrame.Strike(bonus1, bonus2) =>
                state.score + 30 + (10 + 10 + bonus1) + (10 + bonus1 + bonus2)
              case FinalFrame.Spare(roll, bonus) =>
                state.score + (20 + roll) + (10 + 10) + (10 + bonus)
              case FinalFrame.Open(roll1, roll2) =>
                state.score + (20 + roll1) + (10 + roll1 + roll2) + (roll1 + roll2)
            }
          case Spare =>
            finalFrame match {
              case FinalFrame.Strike(bonus1, bonus2) =>
                state.score + 20 + (10 + bonus1 + bonus2)
              case FinalFrame.Spare(roll, bonus) =>
                state.score + (10 + roll) + (10 + bonus)
              case FinalFrame.Open(roll1, roll2) =>
                state.score + (10 + roll1) + (roll1 + roll2)
            }
        }
      case None =>
        finalFrame match {
          case FinalFrame.Strike(bonus1, bonus2) =>
            state.score + (10 + bonus1 + bonus2)
          case FinalFrame.Spare(roll, bonus) =>
            state.score + (10 + bonus)
          case FinalFrame.Open(roll1, roll2) =>
            state.score + (roll1 + roll2)
        }
    }
  }
}
object Game {
  final case class State(
      score: Int,
      pending: Option[Pending]
  ) {
    def next(additionalScore: Int, newPending: Option[Pending]): State =
      this.copy(
        score = score + additionalScore,
        pending = newPending
      )
  }
  sealed trait Pending extends Product with Serializable
  case object Strike extends Pending
  case object StrikeAndStrike extends Pending
  case object Spare extends Pending

  val initialState = State(0, None)
}
