package code

sealed trait FinalFrame extends Product with Serializable
object FinalFrame {
  final case class Strike(bonus1: Int, bonus2: Int) extends FinalFrame
  final case class Spare(roll: Int, bonus: Int) extends FinalFrame
  final case class Open(roll1: Int, roll2: Int) extends FinalFrame

  def strike(bonus1: Int, bonus2: Int): FinalFrame =
    if (bonus1 >= 0 && bonus1 <= 10 && bonus2 >= 0 && bonus2 <= 10)
      Strike(bonus1, bonus2)
    else
      throw new IllegalArgumentException(
        s"Cannot construct a strike final frame with bonus rolls $bonus1 and $bonus2"
      )

  def spare(roll: Int, bonus: Int): FinalFrame =
    if (roll >=0 && roll < 10 && bonus >= 0 && bonus <= 10)
      Spare(roll, bonus)
    else
      throw new IllegalArgumentException(
        s"Cannot construct a spare final frame with roll $roll and bonus roll $bonus"
      )

  def open(roll1: Int, roll2: Int): FinalFrame =
    if (roll1 >= 0 && roll2 >= 0 && (roll1 + roll2) < 10)
      Open(roll1, roll2)
    else
      throw new IllegalArgumentException(
        s"Cannot construct a open final frame with rolls $roll1 and $roll2"
      )
}
